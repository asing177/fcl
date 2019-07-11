module Test.Workflow.Generation.SafeWorkflowNet.Tests
  ( module Test.Workflow.Generation.SafeWorkflowNet.Tests
  ) where

import Protolude

import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.HUnit

import Language.FCL.Pretty
import Language.FCL.ReachabilityGraph

import Test.Workflow.Generation.SafeWorkflowNet
import Test.Workflow.Generation.SafeWorkflowNet.Examples

isSafeWorkflowSound :: SafeWorkflowNet -> Bool
isSafeWorkflowSound = null . soundnessCheck

soundnessCheck :: SafeWorkflowNet -> [WFError]
soundnessCheck = S.toList
               . fst
               . reachabilityGraph
               . S.fromList
               . constructTransitions

mkSoundnessTest :: SafeWorkflowNet -> [Char] -> TestTree
mkSoundnessTest swf name = testCase name $ do
  let errs = soundnessCheck swf
  assertBool (show . vsep . map ppr $ errs) (null errs)

basicNetTests :: [TestTree]
basicNetTests = map (uncurry mkSoundnessTest) namedBasicNets

exampleNetTests :: [TestTree]
exampleNetTests = map (uncurry mkSoundnessTest) namedExampleNets
