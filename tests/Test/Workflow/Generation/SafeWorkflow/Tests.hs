module Test.Workflow.Generation.SafeWorkflow.Tests
  ( isSafeWorkflowSound_FreeChoice
  , isSafeWorkflowSound_General
  , basicNetTests
  , exampleNetTests
  ) where

import Protolude

import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.HUnit

import Language.FCL.AST (Transition)
import Language.FCL.Pretty (Pretty(..), vsep)
import Language.FCL.Reachability.General (completeReachabilityGraph)
import Language.FCL.Reachability.FreeChoice (reachabilityGraph)
import Language.FCL.Reachability.Definitions (WFError, ReachabilityGraph)

import Test.Workflow.Generation.SafeWorkflow
import Test.Workflow.Generation.SafeWorkflow.Examples

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
