module Test.Workflow.SafeWorkflow.Tests
  ( isSafeWorkflowSound_FreeChoice
  , isSafeWorkflowSound_General
  , basicNetTests
  , exampleNetTests
  , bothYieldSameDecision
  ) where

import Protolude

import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.HUnit

import Language.FCL.AST (Transition)
import Language.FCL.Pretty (Pretty(..), vsep)
import Language.FCL.Reachability.General (completeReachabilityGraph)
import Language.FCL.Reachability.FreeChoice (reachabilityGraph)
import Language.FCL.Reachability.Definitions (WFError(..), ReachabilityGraph)

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
  assertBool (show . vsep . map ppr $ errs) (null errs)

basicNetTests :: [TestTree]
basicNetTests = map (uncurry mkFCSoundnessTest) namedBasicNets

exampleNetTests :: [TestTree]
exampleNetTests = map (uncurry mkFCSoundnessTest) namedExampleNets

-----------------------------
-- Extended safe workflows --
-----------------------------

-- | Checks whether both analyses yield the same decision
-- for a given extended safe workflow net.
-- If the extra transitions introduced for the net violate
-- the free choice property, the answer is `True`.
bothYieldSameDecision :: ExtendedSW -> Bool
bothYieldSameDecision esw
  | (freeChoiceErrs, generalErrs) <- checkWithBoth esw
  , fcDecision  <- null freeChoiceErrs
  , genDecision <- null generalErrs
  = containsNotFCErr freeChoiceErrs || fcDecision == genDecision

containsNotFCErr :: [WFError] -> Bool
containsNotFCErr errs = not . null $ [ fcErr | fcErr@(FreeChoiceViolation _ _ _) <- errs ]

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
