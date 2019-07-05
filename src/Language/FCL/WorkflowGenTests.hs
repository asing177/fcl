module Language.FCL.WorkflowGenTests
  ( module Language.FCL.WorkflowGenTests
  ) where

import Protolude

import qualified Data.Set as S

import Language.FCL.Pretty
import Language.FCL.ReachabilityGraph
import Language.FCL.WorkflowGenExamples

isSafeWorkflowSound :: SafeWorkflowNet -> Bool
isSafeWorkflowSound = null
                    . fst
                    . reachabilityGraph
                    . S.fromList
                    . constructTransitions

soundnessCheck :: SafeWorkflowNet -> [WFError]
soundnessCheck = S.toList
               . fst
               . reachabilityGraph
               . S.fromList
               . constructTransitions

basicNetsAreSound :: [[Char]]
basicNetsAreSound = map snd
                  . filter (not . isSafeWorkflowSound . fst)
                  $ namedBasicNets

exampleNetsAreSound :: [[Char]]
exampleNetsAreSound = map snd
                    . filter (not . isSafeWorkflowSound . fst)
                    $ namedExampleNets

arbitraryNetsAreSound :: [[Char]]
arbitraryNetsAreSound = map snd
                      . filter (not . isSafeWorkflowSound . fst)
                      $ namedArbitraryNets
