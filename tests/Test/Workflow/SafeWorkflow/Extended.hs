{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Test.Workflow.SafeWorkflow.Extended where

import Protolude

import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (catMaybes)

import Language.FCL.AST (Transition(..), WorkflowState(..), Place(..), unsafeWorkflowState, wfIntersection)
import Language.FCL.Analysis (inferStaticWorkflowStates)
import Language.FCL.SafeWorkflow (SafeWorkflow(..), constructTransitions)
import Language.FCL.SafeWorkflow.Simple (SimpleSafeWorkflow)

import Test.QuickCheck

-- | Safe workflow extended with some additional places, states and transitions.
data ExtendedSW
  = ExtendedSW { eswSafeWorkflow      :: SimpleSafeWorkflow  -- ^ Basic safe workflow
               , eswExtraTransitions  :: Set Transition      -- ^ Additional transitions (can contain new states)
               }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Return the transitions associated with an
-- extended safe worklow. The original transitions
-- appear earlier in the list than the newly added ones.
extendedWorkflowTransitions :: ExtendedSW -> [Transition]
extendedWorkflowTransitions (ExtendedSW swf trs) = nub $ (S.toList trs) ++ (constructTransitions swf)

{- distribution based on original number of places?
  generate "safe" transitions (net stays free choice)
  split definitions into: place generation, state generation, transition generation (make them parameterizable)
  transitions:
    - FC property
    - XORs and ANDs
-}
instance Arbitrary ExtendedSW where
  arbitrary = do
    swf <- arbitrary @SimpleSafeWorkflow
    let origTrans  = constructTransitions swf                       :: [Transition]
        origStates = S.toList $ inferStaticWorkflowStates origTrans :: [WorkflowState]
        origPlaces = S.toList $ foldMap places origStates           :: [Place]

        -- (frequency out of 100, number of new places to be generated)
        baseDistribution :: Distribution
        baseDistribution = [ (30, 0)
                           , (25, 1)
                           , (20, 2)
                           , (10, 3)
                           , (5,  4)
                           , (3,  5)
                           , (3,  6)
                           , (2,  7)
                           , (2,  8)
                           ]

    let -- | Generate a new place.
        arbNewPlace :: Gen Place
        arbNewPlace = (arbitrary @Place) `suchThat` (`notElem` origPlaces)

    extraPlaces <- severalUnique arbNewPlace `withDistribution` baseDistribution

    let allPlaces = origPlaces ++ (S.toList extraPlaces)

        -- | Generate a new workflow state that has less than 3 places.
        mArbNewState :: Gen (Maybe WorkflowState)
        mArbNewState = do
          let origStatePlaces = map places origStates
          mNewPlaceSet <- sublistOf allPlaces `suchThatMaybe` \ps ->
            length ps <= 3 && (S.fromList ps) `notElem` origStatePlaces
          pure $ (unsafeWorkflowState . S.fromList) <$> mNewPlaceSet

        newStateDist :: Distribution
        newStateDist = map (fmap (+1)) baseDistribution

    extraStates <- someUnique mArbNewState `withDistribution` newStateDist

    -- TODO: way too general, has to introduce more specific transition generators
    let allStates = origStates ++ (S.toList extraStates)

        -- | Generate a new transition.
        mArbNewTrans :: Gen (Maybe Transition)
        mArbNewTrans = (Arrow <$> elements allStates <*> elements allStates) `suchThatMaybe` (`notElem` origTrans)

        newTransDist :: Distribution
        newTransDist = map (fmap (+1)) baseDistribution

    extraTransitions <- someUnique mArbNewTrans `withDistribution` newTransDist

    pure $ ExtendedSW swf extraTransitions

  -- TODO: probably should remove extra places and states, then shrinking would not need to track them
  shrink eSwf@ExtendedSW{..}
    -- after we shrank all the extra transitions we are done
    | null eswExtraTransitions = []
    | otherwise = [ eSwf { eswExtraTransitions = shrankTrs } | shrankTrs <- shrink eswExtraTransitions ]


-- | Distribution given with frequencies
type Distribution = [(Int, Int)]

-- | Generate a number of things using a given distribution.
-- The TODO: finish
withDistribution :: (Int -> Gen (Set a)) -> Distribution -> Gen (Set a)
withDistribution genSome = frequency . map (fmap genSome)


-------------------------
-- Directed generation --
-------------------------

-- | Generates exactly `n` number of entities, then returns only the unique ones.
severalUnique :: Ord a => Gen a -> Int -> Gen (Set a)
severalUnique g n = S.fromList <$> vectorOf n g

-- | Tries to generate `n` number of entities, but might not be able to.
-- Then returns only the unique ones.
someUnique :: Ord a => Gen (Maybe a) -> Int -> Gen (Set a)
someUnique g n = (S.fromList . catMaybes) <$> vectorOf n g

-- TODO: might not be needed
-- | Generate a new `Place` different from some already existing ones.
arbNewPlace :: Set Place -> Gen Place
arbNewPlace origPlaces = (arbitrary @Place) `suchThat` (`notElem` origPlaces)

-- TODO: might not be needed
-- | Tries to generate a new `WorkflowState` different from the already existing ones.
-- The new state will contain `Place`s chosen from a given set. If finding such a
-- state would take longer than a certain time limit, the function will return `Nothing`.
mArbNewState :: Set Place -> Set WorkflowState -> Gen (Maybe WorkflowState)
mArbNewState (S.toList -> allPlaces) (S.toList -> origStates) = do
  let origStatePlaces = map places origStates
  mNewPlaceSet <- sublistOf allPlaces `suchThatMaybe` \ps ->
    not (null ps) && length ps <= 3 && (S.fromList ps) `notElem` origStatePlaces
  pure $ (unsafeWorkflowState . S.fromList) <$> mNewPlaceSet

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

isSingletonState :: WorkflowState -> Bool
isSingletonState = isSingleton . S.toList . places

-- | Tries to generate a transition which goes from singleton workflow state
-- to another singleton workflow state. The two workflow states
-- will always be different. If finding such a transition would take longer
-- than a certain time limit, the function will return `Nothing`.
arbSingleTransition :: Set WorkflowState -> Gen (Maybe Transition)
arbSingleTransition wfs = do
  let singletonStates :: Set WorkflowState
      singletonStates = S.filter isSingletonState wfs

      nonLooping :: Transition -> Bool
      nonLooping (Arrow lhs rhs) = lhs /= rhs

  nonLooping `arbTransitionFrom` singletonStates

-- | Tries to generate a transition which goes from singleton workflow state
-- to another singleton workflow state. The two workflow states
-- can be the same. If finding such a transition would take longer
-- than a certain time limit, the function will return `Nothing`.
arbAnySingleTransition :: Set WorkflowState -> Gen (Maybe Transition)
arbAnySingleTransition wfs = do
  let singletonStates :: Set WorkflowState
      singletonStates = S.filter isSingletonState wfs

      any :: Transition -> Bool
      any = const True

  any `arbTransitionFrom` singletonStates

-- | Tries to generate an arbitrary transition from a given set of
-- workflow states which satisfies a given predicate.
-- If finding such a transition would take longer
-- than a certain time limit, the function will return `Nothing`.
arbTransitionFrom :: (Transition -> Bool) -> Set WorkflowState -> Gen (Maybe Transition)
arbTransitionFrom p (S.toList -> wfs) =
  (Arrow <$> elements wfs <*> elements wfs) `suchThatMaybe` p

-- | Tries to generate a new transition that does not violate the free choice
-- property of the workflow.
arbFreeChoiceTransition :: Set Transition -> Set WorkflowState -> Gen (Maybe Transition)
arbFreeChoiceTransition trSet (S.toList -> wfs) = do
  let inputStates = S.map (\(Arrow lhs _) -> lhs) trSet
      wfDisjoint lhs rhs = null $ places $ wfIntersection lhs rhs
  mLhs <- elements wfs `suchThatMaybe` \s ->
    s `elem` inputStates || all (wfDisjoint s) inputStates
  case mLhs of
    Nothing  -> pure Nothing
    Just lhs -> (Arrow <$> pure lhs <*> elements wfs) `suchThatMaybe` (`notElem` trSet)

newtype ExtendedFCSW = EFCSW { fcGetESW :: ExtendedSW }
  deriving (Eq, Ord, Show, Generic, NFData)

instance Arbitrary ExtendedFCSW where
  arbitrary = do
    swf <- arbitrary @SimpleSafeWorkflow
    let origTrans  = constructTransitions swf                       :: [Transition]
        origStates = S.toList $ inferStaticWorkflowStates origTrans :: [WorkflowState]
        origPlaces = S.toList $ foldMap places origStates           :: [Place]

        -- (frequency out of 100, number of new places to be generated)
        baseDistribution :: Distribution
        baseDistribution = [ (30, 0)
                           , (25, 1)
                           , (20, 2)
                           , (10, 3)
                           , (5,  4)
                           , (3,  5)
                           , (3,  6)
                           , (2,  7)
                           , (2,  8)
                           ]

        arbNewPlace' :: Gen Place
        arbNewPlace' = arbNewPlace (S.fromList origPlaces)

    extraPlaces <- severalUnique arbNewPlace' `withDistribution` baseDistribution

    let allPlaces = origPlaces ++ (S.toList extraPlaces)

        mArbNewState' :: Gen (Maybe WorkflowState)
        mArbNewState' = mArbNewState (S.fromList allPlaces) (S.fromList origStates)

        newStateDist :: Distribution
        newStateDist = map (fmap (+1)) baseDistribution

    extraStates <- someUnique mArbNewState' `withDistribution` newStateDist

    let allStates = origStates ++ (S.toList extraStates)

        -- TODO: review
        -- | Generate a new transition.
        genNewTransition :: Set Transition -> Gen (Set Transition)
        genNewTransition acc = do
          mTr <- arbFreeChoiceTransition (acc <> (S.fromList origTrans)) (S.fromList allStates)
          pure . S.union acc . S.fromList . maybeToList $ mTr

        someFCTransitions :: Int -> Gen (Set Transition)
        someFCTransitions n = foldM (\acc _ -> genNewTransition acc) mempty [1..n]

        newTransDist :: Distribution
        newTransDist = map (fmap (+1)) baseDistribution

    extraTransitions <- someFCTransitions `withDistribution` newTransDist

    pure $ EFCSW $ ExtendedSW swf extraTransitions

  shrink (EFCSW esw) = [ EFCSW esw' | esw' <- shrink esw ]
