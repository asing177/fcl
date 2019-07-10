module TestSoundness where

import Protolude

import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.QuickCheck

import Language.FCL.ReachabilityGraph
import WorkflowGen

isSound :: SafeWorkflowNet -> Bool
isSound = null
        . fst
        . reachabilityGraph
        . S.fromList
        . constructTransitions

soundnessConfirmation :: TestTree
soundnessConfirmation = testGroup ("Test the soundness checking algorithm on safely constructed workflows")
  [ testProperty "No errors while constructing the reachability graph" (withMaxSuccess 1000 isSound)
  ]
