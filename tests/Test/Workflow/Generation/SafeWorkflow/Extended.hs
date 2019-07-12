{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
module Test.Workflow.Generation.SafeWorkflow.Extended where

import Protolude

import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as S

import Language.FCL.AST (Transition(..), WorkflowState(..), Place(..), unsafeWorkflowState)
import Language.FCL.Analysis (inferStaticWorkflowStates)

import Test.QuickCheck

import Test.Workflow.Generation.SafeWorkflow (SafeWorkflow(..), constructTransitions)

-- | Safe workflow extended with some additional places, states and transitions.
data ExtendedSW
  = ExtendedSW { eswSafeWorkflow      :: SafeWorkflow       -- ^ Basic safe workflow
               , eswExtraPlaces       :: Set Place          -- ^ Additional places                                [TODO: might not be needed]
               , eswExtraStates       :: Set WorkflowState  -- ^ Additional states (can contain new places)       [TODO: might not be needed]
               , eswExtraTransitions  :: Set Transition     -- ^ Additional transitions (can contain new states)
               }
  deriving (Eq, Ord, Show, Generic)

extendedWorkflowTransitions :: ExtendedSW -> Set Transition
extendedWorkflowTransitions (ExtendedSW swf _ _ trs) = S.union trs (S.fromList $ constructTransitions swf)

{- distribution based on original number of places?
  generate "safe" transitions (net stays free choice)
  split definitions into: place generation, state generation, transition generation (make them parameterizable)
  transitions:
    - FC property
    - XORs and ANDs
-}
instance Arbitrary ExtendedSW where
  arbitrary = do
    swf <- arbitrary @SafeWorkflow
    let origTrans  = constructTransitions swf                       :: [Transition]
        origStates = S.toList  $inferStaticWorkflowStates origTrans :: [WorkflowState]
        origPlaces = S.toList $ foldMap places origStates           :: [Place]

        severalUnique :: Ord a => Gen a -> Int -> Gen (Set a)
        severalUnique g n = S.fromList <$> vectorOf n g

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
        arbNewState :: Gen WorkflowState
        arbNewState = do
          let origStatePlaces = map places origStates
          newPlaceSet <- sublistOf allPlaces `suchThat` \ps ->
            length ps <= 3 && (S.fromList ps) `notElem` origStatePlaces
          pure . unsafeWorkflowState . S.fromList $ newPlaceSet

        newStateDist :: Distribution
        newStateDist = map (fmap (+1)) baseDistribution

    extraStates <- severalUnique arbNewState `withDistribution` newStateDist

    -- TODO: way too general, has to introduce more specific transition generators
    let allStates = origStates ++ (S.toList extraStates)

        -- | Generate a new transition.
        arbNewTrans :: Gen Transition
        arbNewTrans = (Arrow <$> elements allStates <*> elements allStates) `suchThat` (`notElem` origTrans)

        newTransDist :: Distribution
        newTransDist = map (fmap (+1)) baseDistribution

    extraTransitions <- severalUnique arbNewTrans `withDistribution` newTransDist

    pure $ ExtendedSW swf extraPlaces extraStates extraTransitions

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