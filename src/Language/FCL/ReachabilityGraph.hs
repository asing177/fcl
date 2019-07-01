{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.FCL.ReachabilityGraph
  ( ReachabilityGraph
  , WFError(..)
  , allPlaces
  , applyTransition
  , checkTransitions
  , pprReachabilityGraph
  , reachabilityGraph
  ) where

import Protolude

import Control.Monad.RWS.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Sq
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Aeson as A

import Language.FCL.AST (Name(..), unsafeWorkflowState, Place(..), Transition(..), WorkflowState, (\\), endState, isSubWorkflow, places, startState, wfIntersection, wfUnion)
import Language.FCL.Pretty (Doc, Pretty(..), (<+>), listOf, squotes, text, vcat)

-- | A reason why the workflow is unsound. (Refer to 'Pretty' instance for
-- explanations.
data WFError
  = NotOneBounded WorkflowState Transition (Set Place)
  | ImproperCompletion WorkflowState Transition WorkflowState
  | TransFromEnd Transition
  | Counreachable WorkflowState
  | UnreachableTransition Transition
  -- TODO: make these more errors more informative
  | NotFreeChoice
  | NotOneBoundedMerge
  | ImproperCompletionMerge
  deriving (Eq, Ord, Show, Generic, A.FromJSON, A.ToJSON)

instance Pretty WFError where
  ppr = \case
      NotOneBounded curr t badGuys
        -> "One-boundedness violation. Applying transition" <+> qp t <+> "to state" <+> qp curr
        <+> "would result in an illegal state where the following"
        <+> case S.toList badGuys of
          [badGuy] -> "place occurs more than once:" <+> qp badGuy <> "."
          badGuys@(_:_) -> "places occur more than once:" <+> (squotes . listOf) badGuys <> "."
          [] -> panic "Error created in error."
      ImproperCompletion curr t newState
        -> "Improper completion. Applying" <+> qp t <+> "to state" <+> qp curr
        <+> "would result in the new state" <+> qp newState
        <+> "where the" <+> ppr PlaceEnd
        <+> "place is reached while other places are still active."
      TransFromEnd t
        -> "Transition from end:" <+> qp t <> "."
      Counreachable st
        -> "Dead-end state:" <+> qp st <> "."
      UnreachableTransition t
        -> "Unreachable:" <+> qp t <> "."
      NotFreeChoice -> text "The workflow does not represent a free choice net"
      NotOneBoundedMerge -> text "The net is not one-bounded (found when merging branches)"
      ImproperCompletionMerge -> text "The net does not terminate properly (found at merge-site)"
    where
      qp :: Pretty a => a -> Doc
      qp = squotes . ppr

instance Pretty [WFError] where
  ppr [] = "The workflow is sound."
  ppr errs@(_:_)
    = vcat . ("Workflow soundness errors:" :) $ map (("•" <+>) . ppr) errs

instance Pretty (Set WFError) where
  ppr = ppr . S.toList


-- | A Workflow reachability graph: maps a state to its immediate neighbours.
type ReachabilityGraph = Map WorkflowState (Set WorkflowState)

-- | Pretty-print a reachability graph.
pprReachabilityGraph :: ReachabilityGraph -> Doc
pprReachabilityGraph
  = vcat
  . (text "Reachability Graph:" :)
  . map (\(k, vs) -> ppr k <+> text "->" <+> (listOf . map ppr . S.toList) vs)
  . M.toList

-- | Look up outgoing transitions from a place. E.g. for `{a, b} -> c` we would
-- map `a` and `b` to `{a, b} -> c`.
type OutgoingTransitions = Map Place (Set Transition)

-- | Reachability graph builder monad
type GraphBuilderM = RWS
  OutgoingTransitions -- R: outgoing transitions for each place in the workflow
  (Set WFError, Set Transition) -- W: errors and used transitions
  BuilderState -- S: the graph we are currently building

-- | Given a set of transitions, check whether they describe a sound workflow.
checkTransitions :: Set Transition -> Either [WFError] ReachabilityGraph
checkTransitions ts = case first S.toList $ reachabilityGraph ts of
    ([], graph) -> Right graph
    (errs@(_:_), _) -> Left errs

-- | Return all places that are mentioned in a set of transitions.
allPlaces :: Set Transition -> Set Place
allPlaces = foldMap (\(Arrow (places -> src) (places -> dst)) -> src <> dst)

-- | Build a (partial) reachability graph. When the set of errors is empty,
-- then the graph is complete and the workflow is sound.
reachabilityGraph :: Set Transition -> (Set WFError, ReachabilityGraph)
reachabilityGraph declaredTransitions
  | isFreeChoiceNet declaredTransitions = (allErrs, graph)
  | otherwise = (S.singleton NotFreeChoice, mempty)

  where
    graph :: ReachabilityGraph
    stateErrs :: Set WFError
    usedTransitions :: Set Transition
    (_, BuilderState graph _, (stateErrs, usedTransitions))
      = runRWS
          (buildGraph startState)      -- Function to run in RWS monad
          outgoing                     -- The static context
          (BuilderState mempty mempty) -- The dynamic context

    allErrs :: Set WFError
    allErrs = mconcat [stateErrs, coreachabilityErrs, unreachableTransitionErrs]
      where
        unreachableTransitionErrs
          = S.map UnreachableTransition $ declaredTransitions S.\\ usedTransitions

        coreachabilityErrs = S.map Counreachable counreachable
          where
            -- We have the set 'allCounreachable', but that is too much
            -- information for the user. We only want to report the "outermost"
            -- offending nodes, since all their predecessors are counreachable
            -- by transitivity. We do a breadth-first graph traversal to achieve
            -- this.
            counreachable :: Set WorkflowState
            counreachable
              | null allCounreachable = mempty
              | otherwise = case go (mempty,mempty) (Sq.singleton startState) of
                  xs
                    | isNonEmpty xs && all (`elem` allCounreachable) xs -> -- do a sanity check
                        xs
                    -- TODO: revisit this
                    | otherwise -> allCounreachable
                        -- panic $ "The workflow is unsound, but so is the soundness check."
                              -- <> show xs <> "\n" <> show allCounreachable
              where
                -- Construct the breadth-first spanning tree of the reachability
                -- graph in order to only report relevant coreachability errors,
                -- i.e. the leaves of the tree. NB: @seen@ refers to the seen
                -- counreachable states, not all seen states.
                go
                  :: (Set WorkflowState, Set WorkflowState)
                  -> Seq WorkflowState
                  -> Set WorkflowState
                go (_,leaves) Empty = leaves -- the counreachable nodes to report
                go (oldSeen, leaves) (nextOnes :|> curr)
                  = case unseenNeighbours of
                      ns@(_:_) ->
                        go (seen, leaves) (Sq.fromList ns <> nextOnes)
                      []
                        | curr `elem` allCounreachable ->
                            go (seen, S.insert curr leaves) nextOnes
                        | otherwise -> -- all neighbours already seen or end state
                            go (seen, leaves) nextOnes
                  where
                    seen :: Set WorkflowState
                    seen = S.insert curr oldSeen

                    unseenNeighbours :: [WorkflowState]
                    unseenNeighbours
                      = filter (not . (`elem` seen))
                      . S.toList
                      . fromMaybe mempty
                      $ M.lookup curr graph
                                       -- NOTE: may not be safe anymore
                                       -- safe because we only ever go to
                                       -- successor nodes and these have to be
                                       -- in the reachability graph

                allCounreachable, coreachable, reachable :: Set WorkflowState
                allCounreachable = reachable S.\\ coreachable
                coreachable = M.keysSet (coreachabilityGraph graph)
                reachable = M.keysSet graph


    -- Map nodes to all the outgoing transitions that mention that node in their
    -- left-hand-side (source)
    outgoing :: OutgoingTransitions
    outgoing = M.fromSet inLHS (allPlaces declaredTransitions)
      where
        inLHS :: Place -> Set Transition
        inLHS k = S.filter (\(Arrow src _) -> k `S.member` places src) declaredTransitions

-- Given a reachability graph, make a coreachability graph:
-- 1. reverse all the arrows in the reachability graph
-- 2. traverse the graph from the end state and collect all reachable states
coreachabilityGraph :: ReachabilityGraph -> ReachabilityGraph
coreachabilityGraph rGraph = go endState mempty
  where
    go :: WorkflowState -> ReachabilityGraph -> ReachabilityGraph
    go st acc
        | st `M.member` acc = acc
        | otherwise = foldr go newAcc neighbours
      where
        neighbours = M.findWithDefault mempty st flippedArrows
        newAcc = M.insert st neighbours acc
        flippedArrows = M.fromListWith (<>)
            [ (dst, S.singleton src)
            | (src, dsts) <- M.toList rGraph
            , dst <- S.toList dsts
            ]

--------------------------------------------------------------------------------
-- Convenience functions, these are not exported.
--------------------------------------------------------------------------------

-- TODO: After AND split, we should only record backward transitions
--       when they go "outside" of the branch. If they point back to the same branch,
--       they are irrelevant for the other branches.
--       See: gas-forward
-- Do a depth-first search of the possible states
buildGraph :: WorkflowState -> GraphBuilderM [WorkflowState]
buildGraph curr = do
  unvisited <- unvisitedM curr

  locals <- gets bsLocallyVisted
  let isLocal = curr `S.member` locals
  let g = map ppr . toList . places
  trace (show ((g curr, not unvisited, isLocal, concatMap g $ toList locals) ) :: [Char]) $ pure ()

  if unvisited then do
    outgoing <- ask
    xorSplitStates <- catMaybes <$>
      -- NOTE: XOR-split here
      ( mapM (applyTransitionLclM curr)   -- Get the valid neighbouring states
      . S.toList
      . S.unions                       -- Throw out duplicate transitions
      . mapMaybe (`M.lookup` outgoing) -- Get potentially relevant transitions
      . S.toList
      . places                         -- Get the places in the current state
      ) curr

    addLocalWfState curr
    modifyGraph $ M.insert curr (S.fromList xorSplitStates) -- direct connections
    -- modify . M.insert curr . S.fromList $ neighbours -- Even if there are none
    -- mapM_ buildGraph neighbours
    xorSplitResult <- forM xorSplitStates $ \lclSt -> do
      let andSplitLocalStates = splitState lclSt
      -- NOTE: clear before a branch
      -- when (not $ isSingleton andSplitLocalStates) $
      --   clearLocalWfStates
      let f = if isSingleton andSplitLocalStates then
                buildGraph
              else
                buildLocally
      individualResults <- mapM f andSplitLocalStates
      mergedResult      <- foldlM (cartesianWithM checkedWfUnionM) [mempty] individualResults
      unvisited <- unvisitedM lclSt
      when unvisited $
        modifyGraph $ M.insert lclSt (S.fromList mergedResult)

      let isNewMergedState = concat individualResults /= mergedResult -- any (not . isSingleton) individualResults && (not $ isSingleton individualResults)
      pure (mergedResult, isNewMergedState)


    let noApplicableTranstions = null xorSplitStates

    if null xorSplitStates then
    -- NOTE: the current branch got stuck
      pure [curr]
    else do
      let h (rs,True)  = buildGraph (mconcat rs)
          -- NOTE: The current branch didnt get stuck,
          -- but we didnt get a new by merging the branch results.
          h (rs,False) = pure rs
      concatMapM h xorSplitResult

  else do
    locals <- gets bsLocallyVisted
    if curr `S.member` locals then
      pure []
    else
      pure [curr]

unvisitedM :: WorkflowState -> GraphBuilderM Bool
unvisitedM s = do
  graph <- gets bsReachabilityGraph
  pure $ s `M.notMember` graph

-- Apply a transition if it is satisfied, but if the transition couldn't fire, return the original state
applyTransitionLclM :: WorkflowState ->
                       Transition ->
                       GraphBuilderM (Maybe WorkflowState)
applyTransitionLclM st t = case applyTransition st t of
  Nothing -> pure Nothing -- (Just st)
  Just (Right st) -> do
    tell (mempty, S.singleton t) -- add transition to used
    pure $ Just st
  Just (Left err) -> do
    yell err
    case err of
      ImproperCompletion _ t _ -> tell (mempty, S.singleton t)
      -- add transition to used to avoid UnreachableTransition error
      _ -> pure ()
    pure Nothing
{-# INLINE applyTransitionLclM #-}

yell :: WFError -> GraphBuilderM ()
yell err = tell (S.singleton err, mempty)
{-# INLINE yell #-}

-- TODO: gather all errors
checkedWfUnion :: WorkflowState -> WorkflowState -> Either WFError WorkflowState
checkedWfUnion lhs rhs
  | not $ null sharedPlaces = Left NotOneBoundedMerge
  | PlaceEnd `S.member` lhsPlaces || PlaceEnd `S.member` rhsPlaces
  , S.size (lhsPlaces `S.union` rhsPlaces) > 1
  = Left NotOneBoundedMerge
  | otherwise = Right (lhs <> rhs)
  where sharedPlaces = lhsPlaces `S.intersection` rhsPlaces
        lhsPlaces = places lhs
        rhsPlaces = places rhs

-- QUESTION: what state should it return?
checkedWfUnionM :: WorkflowState -> WorkflowState -> GraphBuilderM WorkflowState
checkedWfUnionM lhs rhs = case checkedWfUnion lhs rhs of
  Right wfs -> pure wfs
  Left  err -> yell err >> pure (lhs <> rhs)

-- TODO: Update docs
-- NOTE: AND-split handled here
-- | Given a current global workflow state, apply a (local) transition.
-- Returns:
--   - @Nothing@ if the transition is not satisfied.
--   - @Left err@ if the transition is satisfied but would lead to an invalid state.
--   - @Right newWorklowState@ otherwise.
applyTransition :: WorkflowState ->
                    Transition ->
                    Maybe (Either WFError WorkflowState)  -- global, local state
applyTransition curr t@(Arrow src dst)
  | not (src `isSubWorkflow` curr) = Nothing -- can't fire transition
  | src == endState                = Just . Left $ TransFromEnd t
  | isNonEmpty sharedPlaces        = Just . Left $ NotOneBounded curr t sharedPlaces
  | PlaceEnd `elem` places newState && newState /= endState
    = Just . Left $ ImproperCompletion curr t newState
  | otherwise = Just $ Right newState
  where
    sharedPlaces :: Set Place
    sharedPlaces = places $ (curr \\ src) `wfIntersection` dst

    newState :: WorkflowState
    newState = (curr \\ src) `wfUnion` dst

{-# INLINE applyTransition #-} -- very important for performance!!

isNonEmpty :: Foldable f => f a -> Bool
isNonEmpty = not . null

isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _   = False

-- | Determines whether a set of transitions representing a workflow
-- satisfy the free choice property.
isFreeChoiceNet :: Set Transition -> Bool
isFreeChoiceNet (S.toList -> trs)
  = and [ satisfyFreeChoiceProperty lhs rhs | lhs <- trs, rhs <- trs ]

-- | Determines whether a pair of transitions satisfy the free choice property.
satisfyFreeChoiceProperty :: Transition -> Transition -> Bool
satisfyFreeChoiceProperty (Arrow lhs _) (Arrow rhs _)
  | lhsPreset <- places lhs
  , rhsPreset <- places rhs
  = S.disjoint lhsPreset rhsPreset || lhsPreset == rhsPreset

cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianWith f xs ys = [f x y | x <- xs, y <- ys]

cartesianWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
cartesianWithM f xs = sequence . cartesianWith f xs

setCartesianWith :: Ord c => (a -> b -> c) -> Set a -> Set b -> Set c
setCartesianWith f xs ys = S.fromList $ cartesianWith f lxs lys
  where lxs = S.toList xs
        lys = S.toList ys

-- | Splits a workflow state into several workflow states
--  based n the contained places. It cretaes a new state
-- for each individual place.
splitState :: WorkflowState -> [WorkflowState]
splitState = map unsafeWorkflowState
           . map S.singleton
           . S.toList
           . places

type LocallyVisited = Set WorkflowState

data BuilderState
  = BuilderState
  { bsReachabilityGraph :: ReachabilityGraph
  , bsLocallyVisted    :: LocallyVisited
  } deriving (Eq, Ord, Show)

modifyGraph :: (ReachabilityGraph -> ReachabilityGraph) -> GraphBuilderM ()
modifyGraph f = do
  BuilderState rg lv <- get
  put $ BuilderState (f rg) lv

modifyLocallyVisited :: (LocallyVisited -> LocallyVisited) -> GraphBuilderM ()
modifyLocallyVisited f = do
  BuilderState rg lv <- get
  put $ BuilderState rg (f lv)

addLocalWfState :: WorkflowState -> GraphBuilderM ()
addLocalWfState s = modifyLocallyVisited (S.insert s)

clearLocalWfStates :: GraphBuilderM ()
clearLocalWfStates = modifyLocallyVisited (const mempty)

locally :: MonadState s m => s -> m a -> m (a,s)
locally s m = do
  origState <- get
  put s
  res <- m
  newState <- get
  put origState
  pure (res, newState)

buildLocally :: WorkflowState -> GraphBuilderM [WorkflowState]
buildLocally wfs = do
  BuilderState rg lv <- get
  (r,s) <- locally (BuilderState rg mempty) (buildGraph wfs)
  modifyGraph (<> bsReachabilityGraph s)
  pure r

