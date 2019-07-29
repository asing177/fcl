{-# LANGUAGE ViewPatterns #-}
module Test.Workflow.SafeWorkflow.Tests
  ( isSafeWorkflowSound_SplitAndMerge
  , isSafeWorkflowSound_General
  , basicNetTests
  , exampleNetTests
  , crossValidWitnessNetTests
  , soundButNotSafeWitnessNetTests
  , bothYieldSameDecision
  , bothYieldSameDecisionFC
  , splitAndMergeIsStricter
  , isReallyFreeChoice
  , transitionsLEQSize
  , samGraphIsSmaller_SW
  , samGraphIsSmaller_ESW
  , structureUnstructureId_Safe
  , structureUnstructureId_Extended
  , structureUnstructureSimplyId_Safe
  , structureUnstructureSimplyId_Extended

  -- TODO: remove these
  , checkWithBoth
  ) where

import Protolude

import qualified Data.Set as S
import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import Test.QuickCheck (Gen(..), Arbitrary(..), NonNegative(..), Small(..), resize)

import Language.FCL.AST (Transition, startState)
import Language.FCL.Pretty (Pretty(..), (<$$>), vsep, linebreak)
import Language.FCL.Reachability.General (completeReachabilityGraph)
import Language.FCL.Reachability.SplitAndMerge (reachabilityGraph, freeChoicePropertyViolations)
import Language.FCL.Reachability.Definitions (WFError(..), ReachabilityGraph)
import Language.FCL.Reachability.Utils (gatherReachableStatesFrom)
import Language.FCL.Reachability.StructuredTransition (structureTransitions, unstructureTransitions, structureSimply, unstructureSimply)

import Test.Workflow.SafeWorkflow
import Test.Workflow.SafeWorkflow.Extended
import Test.Workflow.SafeWorkflow.Examples

-- TODO: probably should separate extended safe workflow test realted stuff from safe workflow related stuff
-- even more nested modules then? ...SafeWrokflow.Extended.Tests ?

--------------------
-- Safe workflows --
--------------------

isSafeWorkflowSound_SplitAndMerge :: SafeWorkflow -> Bool
isSafeWorkflowSound_SplitAndMerge = null . soundnessCheckWith reachabilityGraph

isSafeWorkflowSound_General :: SafeWorkflow -> Bool
isSafeWorkflowSound_General = null . soundnessCheckWith completeReachabilityGraph

soundnessCheckWith :: (Set Transition -> (Set WFError, ReachabilityGraph)) -> SafeWorkflow -> [WFError]
soundnessCheckWith constructGraph
  = S.toList
  . fst
  . constructGraph
  . S.fromList
  . constructTransitions

mkFCSoundnessTest :: SafeWorkflow -> [Char] -> TestTree
mkFCSoundnessTest swf name = testCase name $ do
  let errs = soundnessCheckWith reachabilityGraph swf
  assertBool (show . ppr $ errs) (null errs)

mkCrossValidationTest :: ExtendedFCSW -> [Char] -> TestTree
mkCrossValidationTest esw name = testCase name $ do
  let (fcErrs, genErrs) = checkWithBoth (fcGetESW esw)
      fcDecision  = null fcErrs
      genDecision = null genErrs
  assertBool (show $ ppr fcErrs <$$> linebreak <> "--- (got ^^^) ----- (expected ˇˇˇ) ---" <> linebreak <$$> ppr genErrs) (fcDecision == genDecision)

mkExpectedFailureTest :: ExtendedFCSW -> [Char] -> TestTree
mkExpectedFailureTest esw name = testCase name $ do
  let errs = fst . checkWithBoth . fcGetESW $ esw
  assertBool "Split-and-Merge should have rejected this" (not $ null errs)

basicNetTests :: [TestTree]
basicNetTests = map (uncurry mkFCSoundnessTest) namedBasicNets

exampleNetTests :: [TestTree]
exampleNetTests = map (uncurry mkFCSoundnessTest) namedExampleNets

crossValidWitnessNetTests :: [TestTree]
crossValidWitnessNetTests = map (uncurry mkCrossValidationTest) namedCrossValidWitnessNets

soundButNotSafeWitnessNetTests :: [TestTree]
soundButNotSafeWitnessNetTests = map (uncurry mkExpectedFailureTest) namedSoundButNotSafeWitnessNets

-----------------------------
-- Extended safe workflows --
-----------------------------

-- TODO: --quickcheck-replay=88585
-- | Checks whether both analyses yield the same decision
-- for a given extended safe workflow net.
-- If the extra transitions introduced for the net violate
-- the free choice property, the answer is `True`.
bothYieldSameDecision :: ExtendedSW -> Bool
bothYieldSameDecision esw
  | (freeChoiceErrs, generalErrs) <- checkWithBoth esw
  , fcDecision  <- null freeChoiceErrs
  , genDecision <- null generalErrs
  = containsFCErr freeChoiceErrs || fcDecision == genDecision

containsFCErr :: [WFError] -> Bool
containsFCErr errs = not . null $ [ fcErr | fcErr@(FreeChoiceViolation _ _ _) <- errs ]

checkWithBoth :: ExtendedSW -> ([WFError], [WFError])
checkWithBoth ExtendedSW{..} = ( getErrors . reachabilityGraph         $ eswTransitions
                               , getErrors . completeReachabilityGraph $ eswTransitions
                               ) where
  getErrors :: (Set WFError, ReachabilityGraph) -> [WFError]
  getErrors = S.toList . fst

  eswTransitions :: Set Transition
  eswTransitions = S.union swTransitions eswExtraTransitions

  swTransitions :: Set Transition
  swTransitions  = S.fromList $ constructTransitions eswSafeWorkflow

-- TODO: move these to a sepearate module
-----------

bothYieldSameDecisionFC :: ExtendedFCSW -> Bool
bothYieldSameDecisionFC efcsw
  | esw <- fcGetESW efcsw
  , (freeChoiceErrs, generalErrs) <- checkWithBoth esw
  , fcDecision  <- null freeChoiceErrs
  , genDecision <- null generalErrs
  = fcDecision == genDecision

splitAndMergeIsStricter :: ExtendedFCSW -> Bool
splitAndMergeIsStricter efcsw
  | esw <- fcGetESW efcsw
  , (freeChoiceErrs, generalErrs) <- checkWithBoth esw
  , fcDecision  <- null freeChoiceErrs
  , genDecision <- null generalErrs
  = not fcDecision || genDecision

isReallyFreeChoice :: ExtendedFCSW -> Bool
isReallyFreeChoice = null
                   . freeChoicePropertyViolations
                   . S.fromList
                   . extendedWorkflowTransitions
                   . fcGetESW

------------

transitionsLEQSize :: NonNegative (Small Int) -> Gen Bool
transitionsLEQSize (getSmall . getNonNegative -> 0) = do
  swf <- resize 0 $ arbitrary :: Gen SafeWorkflow
  return $ swf == Atom
transitionsLEQSize (getSmall . getNonNegative -> n) = do
  swf <- resize n $ arbitrary :: Gen SafeWorkflow
  let trsCount = length $ constructTransitions swf
  return $ trsCount <= n


------------

countStates :: (a, ReachabilityGraph) -> Int
countStates = length . gatherReachableStatesFrom startState . snd

samGraphIsSmaller_SW :: SafeWorkflow -> Bool
samGraphIsSmaller_SW sw = (countStates . reachabilityGraph $ trs) <= (countStates . completeReachabilityGraph $ trs) where
  trs = S.fromList $ constructTransitions sw

-- TODO: move these to a sepearate module
samGraphIsSmaller_ESW :: ExtendedFCSW -> Bool
samGraphIsSmaller_ESW esw = (countStates . reachabilityGraph $ trs) <= (countStates . completeReachabilityGraph $ trs) where
  trs = S.fromList $ extendedWorkflowTransitions $ fcGetESW esw

-- TODO: move these to a sepearate module
--------------

structureUnstructureId :: Set Transition -> Bool
structureUnstructureId trSet = trSet == roundTrip trSet where
  roundTrip = S.fromList
            . unstructureTransitions
            . structureTransitions
            . S.toList

structureUnstructureId_Safe :: SafeWorkflow -> Bool
structureUnstructureId_Safe = structureUnstructureId
                            . S.fromList
                            . constructTransitions

structureUnstructureId_Extended :: ExtendedFCSW -> Bool
structureUnstructureId_Extended = structureUnstructureId
                                . S.fromList
                                . extendedWorkflowTransitions
                                . fcGetESW

--

structureUnstructureSimplyId :: Set Transition -> Bool
structureUnstructureSimplyId trSet = trSet == roundTrip trSet where
  roundTrip = S.fromList
            . unstructureSimply
            . structureSimply
            . S.toList

structureUnstructureSimplyId_Safe :: SafeWorkflow -> Bool
structureUnstructureSimplyId_Safe = structureUnstructureSimplyId
                                  . S.fromList
                                  . constructTransitions

structureUnstructureSimplyId_Extended :: ExtendedFCSW -> Bool
structureUnstructureSimplyId_Extended = structureUnstructureSimplyId
                                      . S.fromList
                                      . extendedWorkflowTransitions
                                      . fcGetESW
