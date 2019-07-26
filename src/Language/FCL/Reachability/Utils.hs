{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE ViewPatterns #-}
module Language.FCL.Reachability.Utils where

import Protolude

import Control.Monad.Writer

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

import Language.FCL.Debug
import Language.FCL.Pretty (ppr)
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
  | otherwise = Just (unsafeApplyTransition curr t)
{-# INLINE applyTransition #-} -- very important for performance!!

-- | Applies a transition to a given workflow state
-- without checking whether the transition could fire at all.
unsafeApplyTransition :: WorkflowState -> Transition -> Either WFError WorkflowState
unsafeApplyTransition curr t@(Arrow src dst)
  | not (src `isSubWorkflow` curr) = panic $ "unsafeApplyTransition: Can't fire '" <> (show $ ppr $ Debug t) <> "' from '" <> (show $ ppr $ Debug curr) <> "'"
  | src == endState                = Left $ TransFromEnd t
  | isNonEmpty sharedPlaces        = Left $ NotOneBounded curr t sharedPlaces
  | PlaceEnd `elem` places newState && newState /= endState
    = Left $ ImproperCompletion curr t newState
  | otherwise = Right newState
    where
      sharedPlaces :: Set Place
      sharedPlaces = places $ (curr \\ src) `wfIntersection` dst

      newState :: WorkflowState
      newState = (curr \\ src) `wfUnion` dst
{-# INLINE unsafeApplyTransition #-}

-- | Checks whether a given transition can be applied to a given state.
-- Returns just an error if it is not applicable.
checkTransition :: WorkflowState -> Transition -> Maybe WFError
checkTransition curr t@(Arrow src dst)
  | not (src `isSubWorkflow` curr) = Nothing
  | src == endState                = Just $ TransFromEnd t
  | isNonEmpty sharedPlaces        = Just $ NotOneBounded curr t sharedPlaces
  | PlaceEnd `elem` places newState && newState /= endState
    = Just $ ImproperCompletion curr t newState
  | otherwise = Nothing
    where
      sharedPlaces :: Set Place
      sharedPlaces = places $ (curr \\ src) `wfIntersection` dst

      newState :: WorkflowState
      newState = (curr \\ src) `wfUnion` dst
{-# INLINE checkTransition #-}

-- | Checks whether a given foldable structures is not empty.
isNonEmpty :: Foldable f => f a -> Bool
isNonEmpty = not . null

-- | Checks whether a given list contains a single element.
isSingletonList :: [a] -> Bool
isSingletonList [x] = True
isSingletonList _   = False

-- | Checks whether a given data structure contains a single element.
isSingleton :: Foldable t => t a -> Bool
isSingleton = isSingletonList . toList

yell :: (MonadWriter (Set WFError, Set Transition) m) => WFError -> m ()
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
