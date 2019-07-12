module WorkflowGenTests
  ( module WorkflowGenTests
  ) where

import Protolude

import qualified Data.Set as S

import Language.FCL.AST
import Language.FCL.Pretty
import Language.FCL.ReachabilityGraph
import Language.FCL.ReachabilityGraphOLD (completeReachabilityGraph)

import WorkflowGen
import WorkflowGenExamples

isSafeWorkflowSound :: SafeWorkflowNet -> Bool
isSafeWorkflowSound = null . soundnessCheck

isSafeWorkflowGenerallySound :: SafeWorkflowNet -> Bool
isSafeWorkflowGenerallySound = null . generalSoundnessCheck

soundnessCheckWith :: (Set Transition -> (Set WFError, ReachabilityGraph)) -> SafeWorkflowNet -> [WFError]
soundnessCheckWith constructGraph = S.toList
                                  . fst
                                  . constructGraph
                                  . S.fromList
                                  . constructTransitions

soundnessCheck :: SafeWorkflowNet -> [WFError]
soundnessCheck = soundnessCheckWith reachabilityGraph

generalSoundnessCheck :: SafeWorkflowNet -> [WFError]
generalSoundnessCheck = soundnessCheckWith completeReachabilityGraph

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
