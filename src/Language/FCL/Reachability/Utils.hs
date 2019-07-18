{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE ViewPatterns #-}
module Language.FCL.Reachability.Utils where

import Protolude

import Control.Monad.Writer

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

import Language.FCL.AST (Place(..), Transition(..), WorkflowState(..), (\\), endState, isSubWorkflow, places, wfIntersection, wfUnion)
import Language.FCL.Reachability.Definitions

-- | Return all places that are mentioned in a set of transitions.
allPlaces :: Set Transition -> Set Place
allPlaces = foldMap (\(Arrow (places -> src) (places -> dst)) -> src <> dst)

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

unsafeApplyTransition :: WorkflowState -> Transition -> Maybe WorkflowState
unsafeApplyTransition curr t@(Arrow src dst)
  | not (src `isSubWorkflow` curr) = Nothing -- can't fire transition
  | otherwise = Just $ (curr \\ src) `wfUnion` dst
{-# INLINE unsafeApplyTransition #-}

-- | Checks whether a given foldable structures is not empty.
isNonEmpty :: Foldable f => f a -> Bool
isNonEmpty = not . null

-- | Checks whether a given list contains a single element.
isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _   = False

yell :: WFError -> GraphBuilderM s ()
yell err = tell (S.singleton err, mempty)
{-# INLINE yell #-}

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
