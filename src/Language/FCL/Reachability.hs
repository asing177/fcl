{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.FCL.Reachability
  ( WFError(..)
  , allPlaces
  , applyTransition
  , checkTransitions
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

import Language.FCL.AST (Place(..), Transition(..), WorkflowState, (\\), endState, isSubWorkflow, places, startState, wfIntersection, wfUnion)
import Language.FCL.Pretty (Doc, Pretty(..), (<+>), listOf, squotes, text, vcat)

-- | A reason why the workflow is unsound. (Refer to 'Pretty' instance for
-- explanations.)
data WFError
  = NotOneBounded WorkflowState Transition (Set Place)
  | ImproperCompletion WorkflowState Transition WorkflowState
  | TransFromEnd Transition
  | Counreachable WorkflowState
  | UnreachableTransition Transition
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
  ReachabilityGraph -- S: the graph we are currently building

-- | Return all places that are mentioned in a set of transitions.
allPlaces :: Set Transition -> Set Place
allPlaces = foldMap (\(Arrow (places -> src) (places -> dst)) -> src <> dst)

-- | Branching processes of petri nets.
type Net       = Set Node
data Node      = CNode Condition | ENode Event    deriving (Eq, Ord)
data Condition = Condition Place (Maybe Event)    deriving (Eq, Ord)
data Event     = Event Transition (Set Condition) deriving (Eq, Ord)

-- | Parents of branching process nodes.
parents :: Node -> Net
parents (CNode (Condition _ me)) = S.map ENode . S.fromList . maybeToList $ me
parents (ENode (Event _ cs))     = S.map CNode cs

-- | Ascendants of branching process nodes.
ascendants :: Node -> Net
ascendants node = S.singleton node <> foldMap ascendants (parents node)

-- | Two nodes x and y are in concurrent relation if they are neither in causal
-- nor in conflict relation.
concurrent :: Node -> Node -> Bool
concurrent x y = not (causal x y || causal y x || conflict x y)
  where
    -- Two nodes x and y are in causal relation if the net contains a path with
    -- at least one arc leading from x to y.
    causal :: Node -> Node -> Bool
    causal x y = x /= y && elem x (ascendants y)
    -- Two nodes x and y are in conflict relation if the net contains two paths
    -- leading to x and y which start at the same place and immediately diverge.
    conflict :: Node -> Node -> Bool
    conflict x y = isNonEmpty
      [(x, y) | x' <- S.toList (ascendants x), y' <- S.toList (ascendants y),
                x' /= y', isNonEmpty (S.intersection (parents x) (parents y))]

-- | Given a set of transitions, check whether they describe a sound workflow,
-- and if the set of errors is empty then the graph is complete.
checkTransitions :: Set Transition -> [WFError]
checkTransitions declaredTransitions = S.toList allErrs

  where
    graph :: ReachabilityGraph
    stateErrs :: Set WFError
    usedTransitions :: Set Transition
    (_, graph, (stateErrs, usedTransitions))
      = runRWS
          (buildGraph startState) -- Function to run in RWS monad
          outgoing                -- The static context
          mempty                  -- The dynamic context

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
                      $ graph M.! curr -- safe because we only ever go to
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

    -- Do a depth-first search of the possible states
    buildGraph :: WorkflowState -> GraphBuilderM ()
    buildGraph curr = do
        graph <- get
        unless (curr `M.member` graph) $ do
          outgoing <- ask
          neighbours <- catMaybes <$>
            ( mapM (applyTransitionM curr)   -- Get the valid neighbouring states
            . S.toList
            . S.unions                       -- Throw out duplicate transitions
            . mapMaybe (`M.lookup` outgoing) -- Get potentially relevant transitions
            . S.toList
            . places                         -- Get the places in the current state
            ) curr
          modify . M.insert curr . S.fromList $ neighbours -- Even if there are none
          mapM_ buildGraph neighbours
      where

        -- Apply a transition if it is satisfied
        applyTransitionM
          :: WorkflowState
          -> Transition
          -> GraphBuilderM (Maybe WorkflowState)
        applyTransitionM st t = case applyTransition st t of
            Nothing -> pure Nothing
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
        {-# INLINE applyTransitionM #-}

        yell :: WFError -> GraphBuilderM ()
        yell err = tell (S.singleton err, mempty)
        {-# INLINE yell #-}

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

-- | Given a current global workflow state, apply a (local) transition.
-- Returns:
--   - @Nothing@ if the transition is not satisfied.
--   - @Left err@ if the transition is satisfied but would lead to an invalid state.
--   - @Right newWorklowState@ otherwise.
applyTransition
  :: WorkflowState
  -> Transition
  -> Maybe (Either WFError WorkflowState)
applyTransition curr t@(Arrow src dst)
    | not (src `isSubWorkflow` curr) = Nothing -- can't fire transition
    | src == endState = Just . Left $ TransFromEnd t
    | isNonEmpty badGuys = Just . Left $ NotOneBounded curr t badGuys
    | PlaceEnd `elem` places newState && newState /= endState
      = Just . Left $ ImproperCompletion curr t newState
    | otherwise = Just $ Right newState
  where
    badGuys :: Set Place
    badGuys = places $ (curr \\ src) `wfIntersection` dst

    newState :: WorkflowState
    newState = (curr \\ src) `wfUnion` dst
{-# INLINE applyTransition #-} -- very important for performance!!


--------------------------------------------------------------------------------
-- Convenience functions, these are not exported.
--------------------------------------------------------------------------------

isNonEmpty :: Foldable f => f a -> Bool
isNonEmpty = not . null
