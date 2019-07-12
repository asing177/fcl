module Test.Workflow.Generation.SafeWorkflowNet.Tests
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
import Language.FCL.ReachabilityGraph (WFError, ReachabilityGraph, reachabilityGraph)
import Language.FCL.ReachabilityGraphOLD (completeReachabilityGraph)

import Test.Workflow.Generation.SafeWorkflowNet
import Test.Workflow.Generation.SafeWorkflowNet.Examples

isSafeWorkflowSound_FreeChoice :: SafeWorkflowNet -> Bool
isSafeWorkflowSound_FreeChoice = null . soundnessCheckWith reachabilityGraph

isSafeWorkflowSound_General :: SafeWorkflowNet -> Bool
isSafeWorkflowSound_General = null . soundnessCheckWith completeReachabilityGraph

soundnessCheckWith :: (Set Transition -> (Set WFError, ReachabilityGraph)) -> SafeWorkflowNet -> [WFError]
soundnessCheckWith constructGraph
  = S.toList
  . fst
  . constructGraph
  . S.fromList
  . constructTransitions

mkFCSoundnessTest :: SafeWorkflowNet -> [Char] -> TestTree
mkFCSoundnessTest swf name = testCase name $ do
  let errs = soundnessCheckWith reachabilityGraph swf
  assertBool (show . vsep . map ppr $ errs) (null errs)

basicNetTests :: [TestTree]
basicNetTests = map (uncurry mkFCSoundnessTest) namedBasicNets

exampleNetTests :: [TestTree]
exampleNetTests = map (uncurry mkFCSoundnessTest) namedExampleNets
