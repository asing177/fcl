{-# LANGUAGE ViewPatterns #-}

module Language.FCL.Reachability.FreeChoice
  ( module Language.FCL.Reachability.Definitions
  , checkTransitions
  , reachabilityGraph
  , freeChoicePropertyViolations
  ) where

import Protolude

import Control.Monad.RWS.Strict

import Data.Set (Set)
import Data.Sequence (Seq(..))

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq

import Language.FCL.AST (unsafeWorkflowState, Place(..), Transition(..), WorkflowState, endState, places, startState)
import Language.FCL.Reachability.Definitions
import Language.FCL.Reachability.Utils


-- | Locally visited nodes inside an AND branch.
type LocallyVisited = Set WorkflowState

-- | State for constructing a reachability graph for a free choice net.
data BuilderState
  = BuilderState
  { bsReachabilityGraph :: ReachabilityGraph  -- ^ The reachability graph itself
  , bsLocallyVisted     :: [LocallyVisited]   -- ^ Stack of local contexts for AND-split branches
  } deriving (Eq, Ord, Show)

-- | Reachability graph builder monad for free choice nets
type FreeChoiceGBM = GraphBuilderM BuilderState

-- | Given a set of transitions, check whether they describe a sound free choice workflow.
checkTransitions :: Set Transition -> Either [WFError] ReachabilityGraph
checkTransitions ts = case first S.toList $ reachabilityGraph ts of
    ([], graph) -> Right graph
    (errs@(_:_), _) -> Left errs


-- | Build a (partial) reachability graph. When the set of errors is empty,
-- then the graph is complete and the workflow is sound. This graph does not
-- contain any intermediate states across AND branches.
reachabilityGraph :: Set Transition -> (Set WFError, ReachabilityGraph)
reachabilityGraph declaredTransitions =
  case freeChoicePropertyViolations declaredTransitions of
    []   -> (allErrs, graph)
    errs -> (S.fromList errs, mempty)

  where
    graph :: ReachabilityGraph
    stateErrs :: Set WFError
    usedTransitions :: Set Transition
    (_, BuilderState graph _, (stateErrs, usedTransitions))
      = runRWS
          (buildGraph startState)      -- Function to run in RWS monad
          outgoing                     -- The static context
          (BuilderState mempty [mempty]) -- The dynamic context

    allErrs :: Set WFError
    allErrs = mconcat [stateErrs, prunedCoreachabilityErrs, unreachableTransitionErrs]
      where
        unreachableTransitionErrs
          = S.map UnreachableTransition $ declaredTransitions S.\\ usedTransitions

        erronousState :: WFError -> Maybe WorkflowState
        erronousState (NotOneBounded wfSt _ _)      = Just wfSt -- ^ incorrect transitions
        erronousState (ImproperCompletion wfSt _ _) = Just wfSt -- ^ incorrect transitions
        erronousState (LoopingANDBranch wfSt _)     = Just wfSt -- ^ erroneous AND branch loops
        erronousState _ = Nothing

        erronousStates :: [WorkflowState]
        erronousStates = mapMaybe erronousState
                       . S.toList
                       $ stateErrs

        -- Exclude those states from where we possibly couldn't transition into another state
        -- because the transition itself was erroneous
        prunedCoreachabilityErrs :: Set WFError
        prunedCoreachabilityErrs = S.fromList $
          [ e | e@(Counreachable wfSt) <- S.toList coreachabilityErrs
              , wfSt `notElem` erronousStates
          ]

        coreachabilityErrs :: Set WFError
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
                    | otherwise ->
                        panic $ "The workflow is unsound, but so is the soundness check."
                              <> show xs <> "\n" <> show allCounreachable
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
                coreachable = gatherReachableStatesFrom endState (coreachabilityGraph graph)
                reachable = gatherReachableStatesFrom startState graph


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

-- | Build the reachability graph of a given workflow net starting
-- from a given workflow state `s`. The return value of the function is
-- the set of possible states reachable directly from `s`
-- (XOR splits can introduce non-determinism). In order to reduce the
-- the number of possible states, AND-splits are treated as a "direct" transition
-- from the input state to the merged result of the individual branch states.
buildGraph :: WorkflowState -> FreeChoiceGBM [WorkflowState]
buildGraph curr = do
  unvisited <- unvisitedM curr

  if unvisited then do
    outgoing <- ask
    xorSplitStates <- catMaybes <$>
      -- NOTE: XOR-split here
      ( mapM (applyTransitionLclM curr)  -- Get the valid neighbouring states
      . S.toList
      . S.unions                         -- Throw out duplicate transitions
      . mapMaybe (`M.lookup` outgoing)   -- Get potentially relevant transitions
      . S.toList
      . places                           -- Get the places in the current state
      ) curr

    modifyGraph $ M.insert curr (S.fromList xorSplitStates) -- direct connections

    -- NOTE: states visited before the XOR split
    visitedStatesBeforeXOR <- gets (M.keysSet . bsReachabilityGraph)

    xorSplitResult <- forM xorSplitStates $ \lclSt -> do
      {- NOTE: states visited before + states visited by other XOR branches
         This changes every time we finish a XOR branch.
      -}
      visitedStatesAfterXOR <- gets (M.keysSet . bsReachabilityGraph)
      let visitedByOtherXORBranches = S.difference visitedStatesAfterXOR visitedStatesBeforeXOR

      let andSplitLocalStates = splitState lclSt
      {- NOTE: keep locally visited when following a straight path,
         reset locally visited when branching
      -}
      let isSimplePath = isSingleton andSplitLocalStates
          buildBranch = if isSimplePath then
                          continueBuildingLocally curr
                        else
                          startBuildingLocally curr
      -- Results of each AND branch.
      -- Each element is a seperate AND branch result.
      -- Inner lists represent possible XOR splits inside a branch.
      -- Here, the results are stored as DIFFERENT workflow states.
      (individualResults :: [[WorkflowState]]) <- mapM buildBranch andSplitLocalStates
      {- NOTE: AND branches that loop back a locally visited state
        return an empty list (and only these). These branches will never
        get joined together with the other branches, or they loop back to
        the splitting point. In both cases, they result in an error.
      -}
      when (not isSimplePath) $ do
        let branches = zip individualResults andSplitLocalStates
            loops    = filter (null . fst) branches
        forM_ loops $ \(_, splitHead) -> do
          yell (LoopingANDBranch lclSt splitHead)
      -- Here the different wf states of the AND branches get merged.
      -- Even if there were no XOR branches inside the AND branches,
      -- the different wf states get merged into a single one.
      (mergedResult :: [WorkflowState]) <- foldlM (cartesianWithM checkedWfUnionM) [mempty] individualResults

      -- NOTE: Only keep those states that hasn't been visited already.
      let mergedResult' = filter (`S.notMember` visitedByOtherXORBranches) mergedResult

      unvisited <- unvisitedM lclSt
      when unvisited $ do
        modifyGraph $ M.insert lclSt (S.fromList mergedResult')

      {- NOTE: They are equal iff there is a single AND branch.
         If it was a simple path, we already visited all the nodes on it.
         However, it there was an AND split whose merge resulted in a new state,
         we must continue exploring from that new merged state.
      -}
      let isNewMergedState = not isSimplePath
      pure (mergedResult', isNewMergedState)


    let noApplicableTranstions = null xorSplitStates
    if noApplicableTranstions then do
    -- NOTE: the current branch got stuck
      pure [curr]
    else do
      -- NOTE: Only continue those XOR branches where the AND branches yielded a new merged state
      let continueIfNew (rs,True)  = concatMapM buildGraph rs
          continueIfNew (rs,False) = pure rs
      concatMapM continueIfNew xorSplitResult

  else do
    locals <- getLocalContext
    if curr `S.member` locals then do
      {- NOTE: Locally visited nodes add no new information to the context.
         This is when a node inside a branch refers back to another node inside the same branch.
      -}
      pure []
    else do
      {- NOTE: Globally visited nodes can have impact on other exectuion paths.
         This is when a node inside a branch refers to another node outside of the current branch.
      -}
      pure [curr]

-- | Checks whether a workflow state is globally unvisited.
unvisitedM :: WorkflowState -> FreeChoiceGBM Bool
unvisitedM s = do
  graph <- gets bsReachabilityGraph
  pure $ s `M.notMember` graph

-- Apply a transition if it is satisfied, but if the transition couldn't fire, return the original state
applyTransitionLclM :: WorkflowState ->
                       Transition ->
                       FreeChoiceGBM (Maybe WorkflowState)
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

-- | Unions two workflow states and gathers all errors that may arise.
checkedWfUnion :: WorkflowState -> WorkflowState -> Either WFError WorkflowState
checkedWfUnion lhs rhs
  | not $ null sharedPlaces = Left (NotOneBoundedMerge lhs rhs sharedPlaces)
  | PlaceEnd `S.member` lhsPlaces || PlaceEnd `S.member` rhsPlaces
  , S.size (lhsPlaces `S.union` rhsPlaces) > 1
  = Left (ImproperCompletionMerge lhs rhs)
  | otherwise = Right (lhs <> rhs)
  where sharedPlaces = lhsPlaces `S.intersection` rhsPlaces
        lhsPlaces = places lhs
        rhsPlaces = places rhs

-- | Unions two workflow states and reports all errors that may arise in a monadic context.
checkedWfUnionM :: WorkflowState -> WorkflowState -> FreeChoiceGBM WorkflowState
checkedWfUnionM lhs rhs = case checkedWfUnion lhs rhs of
  Right wfSt -> pure wfSt
  Left  err  -> yell err >> pure (lhs <> rhs)

-- | Determines whether a set of transitions representing a workflow
-- satisfy the free choice property.
freeChoicePropertyViolations :: Set Transition -> [WFError]
freeChoicePropertyViolations (S.toList -> trs)
  = catMaybes [ sharedInputPlaces lhs rhs
              | (k, lhs) <- zip [0..] trs
              , rhs <- drop k trs
              ]

-- | Determines whether a pair of transitions satisfy the free choice property.
sharedInputPlaces :: Transition -> Transition -> Maybe WFError
sharedInputPlaces t1@(Arrow lhs _) t2@(Arrow rhs _)
  | lhsPreset <- places lhs
  , rhsPreset <- places rhs
  = if S.disjoint lhsPreset rhsPreset || lhsPreset == rhsPreset then
      Nothing
    else
      Just (FreeChoiceViolation t1 t2 (S.intersection lhsPreset lhsPreset))

-- | A function combining every element from the first list
-- with every element in the second list.
cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianWith f xs ys = [f x y | x <- xs, y <- ys]

-- | Monadic version of `cartesianWith`.
cartesianWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
cartesianWithM f xs = sequence . cartesianWith f xs

-- | Splits a workflow state into several workflow states
--  based n the contained places. It cretaes a new state
-- for each individual place.
splitState :: WorkflowState -> [WorkflowState]
splitState = map unsafeWorkflowState
           . map S.singleton
           . S.toList
           . places

-- | General modifying function for the reachability graph.
modifyGraph :: (ReachabilityGraph -> ReachabilityGraph) -> FreeChoiceGBM ()
modifyGraph f = do
  BuilderState rg lv <- get
  put $ BuilderState (f rg) lv

-- | General modifying function for the current local context.
modifyLocalContext :: (LocallyVisited -> LocallyVisited) -> FreeChoiceGBM ()
modifyLocalContext f = do
  BuilderState rg lv <- get
  case lv of
    x:xs -> put $ BuilderState rg (f x : xs)
    -- TODO: might have to reconsider this case
    []   -> pure ()

-- | Creates a new empty local context (for an AND branch).
pushEmptyLocalContext :: FreeChoiceGBM ()
pushEmptyLocalContext = do
  BuilderState rg lv <- get
  put $ BuilderState rg (mempty:lv)

-- | Adds a given state to the current local context.
pushLocalState :: WorkflowState -> FreeChoiceGBM ()
pushLocalState wfSt = modifyLocalContext (S.insert wfSt)

-- | Removes the current local context.
popLocalContext :: FreeChoiceGBM ()
popLocalContext = do
  BuilderState rg lv <- get
  case lv of
    _:xs -> put $ BuilderState rg xs
    []   -> panic $ "Reachability: can't pop from empty context stack"

-- | Acquires the current local context.
getLocalContext :: FreeChoiceGBM LocallyVisited
getLocalContext = do
  BuilderState rg lv <- get
  case lv of
    x:_ -> pure x
    []  -> panic $ "Reachability: can't get locally visited states from empty context stack"

-- | Start analyzing and AND branch by creating a new local context for it.
-- Add the splitting state to the previous context as well to the current one.
startBuildingLocally :: WorkflowState -> WorkflowState -> FreeChoiceGBM [WorkflowState]
startBuildingLocally prev wfSt = do
  BuilderState rg lv <- get
  -- NOTE: have to add it to the previous context as well
  pushLocalState prev
  pushEmptyLocalContext
  -- NOTE: adding it to the currect context
  pushLocalState prev
  r <- buildGraph wfSt
  popLocalContext
  pure r

-- | Continue with the current local context.
-- Add the previously visited state to the current local context.
continueBuildingLocally :: WorkflowState -> WorkflowState -> FreeChoiceGBM [WorkflowState]
continueBuildingLocally prev wfSt = do
  pushLocalState prev
  buildGraph wfSt

-- | Gather all the reachable state from a given state using the reachability graph.
gatherReachableStatesFrom :: WorkflowState -> ReachabilityGraph -> Set WorkflowState
gatherReachableStatesFrom start graph =
  execState (gatherReachableStatesFromM start graph) mempty where

  gatherReachableStatesFromM :: WorkflowState -> ReachabilityGraph -> State (Set WorkflowState) ()
  gatherReachableStatesFromM wfSt graph = do
    s <- get
    when (wfSt `S.notMember` s) $ do
      modify $ S.insert wfSt
      case M.lookup wfSt graph of
        Nothing    -> pure ()
        Just nexts ->
          mapM_ (flip gatherReachableStatesFromM graph) nexts
