module Test.Workflow.SafeWorkflow.Tests
  ( isSafeWorkflowSound_FreeChoice
  , isSafeWorkflowSound_General
  , basicNetTests
  , exampleNetTests
  , witnessNetTests
  , bothYieldSameDecision
  , bothYieldSameDecisionFC
  , isReallyFreeChoice
  , fcGraphIsSmaller_SW
  , fcGraphIsSmaller_ESW

  -- TODO: remove these
  , checkWithBoth
  ) where

import Protolude

import qualified Data.Set as S
import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import Language.FCL.AST (Transition, startState)
import Language.FCL.Pretty (Pretty(..), (<$$>), vsep, linebreak)
import Language.FCL.Reachability.General (completeReachabilityGraph)
import Language.FCL.Reachability.FreeChoice (reachabilityGraph, freeChoicePropertyViolations)
import Language.FCL.Reachability.Definitions (WFError(..), ReachabilityGraph)
import Language.FCL.Reachability.Utils (gatherReachableStatesFrom)

import Test.Workflow.SafeWorkflow
import Test.Workflow.SafeWorkflow.Extended
import Test.Workflow.SafeWorkflow.Examples

-- TODO: probably should separate extended safe workflow test realted stuff from safe workflow related stuff
-- even more nested modules then? ...SafeWrokflow.Extended.Tests ?

--------------------
-- Safe workflows --
--------------------

isSafeWorkflowSound_FreeChoice :: SafeWorkflow -> Bool
isSafeWorkflowSound_FreeChoice = null . soundnessCheckWith reachabilityGraph

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
  assertBool (show $ ppr fcErrs <$$> linebreak <> ppr genErrs) (fcDecision == genDecision)

basicNetTests :: [TestTree]
basicNetTests = map (uncurry mkFCSoundnessTest) namedBasicNets

exampleNetTests :: [TestTree]
exampleNetTests = map (uncurry mkFCSoundnessTest) namedExampleNets

witnessNetTests :: [TestTree]
witnessNetTests = map (uncurry mkCrossValidationTest) namedWitnessNets

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

-----------

bothYieldSameDecisionFC :: ExtendedFCSW -> Bool
bothYieldSameDecisionFC efcsw
  | esw <- fcGetESW efcsw
  , (freeChoiceErrs, generalErrs) <- checkWithBoth esw
  , fcDecision  <- null freeChoiceErrs
  , genDecision <- null generalErrs
  = fcDecision == genDecision

isReallyFreeChoice :: ExtendedFCSW -> Bool
isReallyFreeChoice = null
                   . freeChoicePropertyViolations
                   . S.fromList
                   . extendedWorkflowTransitions
                   . fcGetESW

------------

countStates :: (a, ReachabilityGraph) -> Int
countStates = length . gatherReachableStatesFrom startState . snd

fcGraphIsSmaller_SW :: SafeWorkflow -> Bool
fcGraphIsSmaller_SW sw = (countStates . reachabilityGraph $ trs) <= (countStates . completeReachabilityGraph $ trs) where
  trs = S.fromList $ constructTransitions sw

fcGraphIsSmaller_ESW :: ExtendedFCSW -> Bool
fcGraphIsSmaller_ESW esw = (countStates . reachabilityGraph $ trs) <= (countStates . completeReachabilityGraph $ trs) where
  trs = S.fromList $ extendedWorkflowTransitions $ fcGetESW esw
