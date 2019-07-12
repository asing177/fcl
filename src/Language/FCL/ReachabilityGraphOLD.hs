{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.FCL.ReachabilityGraphOLD
  ( ReachabilityGraph
  , WFError(..)
  , allPlaces
  , applyTransition
  , checkTransitions
  , pprReachabilityGraph
  , completeReachabilityGraph
  ) where

import Protolude

import Control.Monad.RWS.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Sq
import Data.Set (Set)
import qualified Data.Set as S

import Language.FCL.AST (Place(..), Transition(..), WorkflowState, (\\), endState, isSubWorkflow, places, startState, wfIntersection, wfUnion)
import Language.FCL.ReachabilityGraph (ReachabilityGraph, WFError(..), pprReachabilityGraph, allPlaces)

-- | Look up outgoing transitions from a place. E.g. for `{a, b} -> c` we would
-- map `a` and `b` to `{a, b} -> c`.
type OutgoingTransitions = Map Place (Set Transition)

-- | Reachability graph builder monad
type GraphBuilderM = RWS
  OutgoingTransitions -- R: outgoing transitions for each place in the workflow
  (Set WFError, Set Transition) -- W: errors and used transitions
  ReachabilityGraph -- S: the graph we are currently building

-- | Given a set of transitions, check whether they describe a sound workflow.
checkTransitions :: Set Transition -> Either [WFError] ReachabilityGraph
checkTransitions ts = case first S.toList $ completeReachabilityGraph ts of
    ([], graph) -> Right graph
    (errs@(_:_), _) -> Left errs

-- | Build a (partial) reachability graph. When the set of errors is empty,
-- then the graph is complete and the workflow is sound. This graph contains
-- all intermediate workflow states accross AND branches.
completeReachabilityGraph :: Set Transition -> (Set WFError, ReachabilityGraph)
completeReachabilityGraph declaredTransitions = (allErrs, graph)

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
                coreachable = M.keysSet (cocompleteReachabilityGraph graph)
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
cocompleteReachabilityGraph :: ReachabilityGraph -> ReachabilityGraph
cocompleteReachabilityGraph rGraph = go endState mempty
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
