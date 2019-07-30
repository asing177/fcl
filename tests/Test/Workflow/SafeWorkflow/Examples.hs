{-# LANGUAGE PatternSynonyms #-}
module Test.Workflow.SafeWorkflow.Examples
  ( basicNets
  , exampleNets
  , arbitraryNets
  , namedBasicNets
  , namedExampleNets
  , namedArbitraryNets
  , namedCrossValidWitnessNets
  , namedSoundButNotSafeWitnessNets
  ) where

import Protolude

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S

import Language.FCL.AST (Name(..), Place(..), WorkflowState(..), Transition(..), makeWorkflowState, startState, endState, wfUnion)

import Test.Workflow.SafeWorkflow
import Test.Workflow.SafeWorkflow.Extended

-------------------------------------------------
-- Basic building blocks of safe workflow nets --
-------------------------------------------------

namedBasicNets :: [(SafeWorkflow, [Char])]
namedBasicNets = zip basicNets [ "atom"
                               , "XOR"
                               , "AND"
                               , "Seq"
                               , "SimpleLoop"
                               , "Loop"
                               , "compeleteGenXOR"
                               , "toRightGenXOR"
                               , "toLeftGenXOR"
                               , "simpleGenXOR"
                               ]

basicNets :: [SafeWorkflow]
basicNets = [ basicAtom
            , basicXOR
            , basicAND
            , basicSeq
            , basicSimpleLoop
            , basicLoop
            , compeleteGenXOR
            , toRightGenXOR
            , toLeftGenXOR
            , simpleGenXOR
            ]

basicAtom :: SafeWorkflow
basicAtom = Atom

basicXOR :: SafeWorkflow
basicXOR = XOR Atom (XOR Atom Atom)

basicAND :: SafeWorkflow
basicAND = AND (Atom :| [Atom, Atom])

basicSeq :: SafeWorkflow
basicSeq = Seq Atom Atom

basicSimpleLoop :: SafeWorkflow
basicSimpleLoop = SimpleLoop Atom Atom

basicLoop :: SafeWorkflow
basicLoop = Loop Atom Atom Atom

compeleteGenXOR :: SafeWorkflow
compeleteGenXOR = GenXOR Atom Atom Atom Atom (Just Atom) (Just Atom)

toRightGenXOR :: SafeWorkflow
toRightGenXOR = GenXOR Atom Atom Atom Atom (Just Atom) Nothing

toLeftGenXOR :: SafeWorkflow
toLeftGenXOR = GenXOR Atom Atom Atom Atom Nothing (Just Atom)

simpleGenXOR :: SafeWorkflow
simpleGenXOR = GenXOR Atom Atom Atom Atom Nothing Nothing

-------------------------------------------------------------------
-- Already existing examples reproduced using safe workflow nets --
-------------------------------------------------------------------

namedExampleNets :: [(SafeWorkflow, [Char])]
namedExampleNets = zip exampleNets [ "swap"
                                   , "concurrent"
                                   , "amendment"
                                   , "graph"
                                   , "novation"
                                   , "loan-contract"
                                   , "zcb"
                                   , "product"
                                   , "gas-forward-simple"
                                   , "gas-forward"
                                   ]

exampleNets :: [SafeWorkflow]
exampleNets = [ swapNet
              , concurrentNet
              , amendmentNet
              , graphNet
              , novationNet
              , loanContractNet
              , zcbNet
              , productNet
              , gasForwardSimpleNet
              , gasForwardNet
              ]

swapNet :: SafeWorkflow
swapNet = XOR Atom (Loop Atom Atom (XOR Atom Atom))

concurrentNet :: SafeWorkflow
concurrentNet = AND (Atom :| [Atom])

amendmentNet :: SafeWorkflow
amendmentNet = Seq (AND (Atom :| [Atom])) (SimpleLoop (XOR (Seq Atom Atom) Atom) Atom)

graphNet :: SafeWorkflow
graphNet = Seq (XOR (Seq Atom Atom) (Seq Atom Atom)) Atom

-- not 1:1, but kind of "isomorphic"
novationNet :: SafeWorkflow
novationNet = Seq (AND2 Atom Atom) (Loop (AND2 Atom Atom) Atom (AND2 Atom Atom))

loanContractNet :: SafeWorkflow
loanContractNet = Seq Atom (Loop Atom (XOR (Seq Atom (SimpleLoop Atom Atom)) Atom) Atom)

zcbNet :: SafeWorkflow
zcbNet = XOR Atom (Loop Atom (Seq Atom Atom) (XOR Atom Atom))

gasForwardSimpleNet :: SafeWorkflow
gasForwardSimpleNet = Seq Atom (XOR (GenXOR Atom (SimpleLoop Atom Atom) Atom Atom (Just Atom) Nothing) Atom)

gasForwardNet :: SafeWorkflow
gasForwardNet = Seq Atom (XOR (GenXOR Atom (SimpleLoop Atom Atom) Atom gasForwardNominationSubNet (Just Atom) Nothing) Atom) where

  gasForwardNominationSubNet :: SafeWorkflow
  gasForwardNominationSubNet = SimpleLoop Atom (Seq (AND2 Atom (SimpleLoop Atom Atom)) (SimpleLoop Atom Atom))

productNet :: SafeWorkflow
productNet = Seq Atom (XOR (Seq Atom (SimpleLoop (XOR3 Atom Atom Atom) (XOR (Seq Atom (XOR3 (Seq Atom Atom) (Seq Atom Atom) (Seq Atom Atom))) (Loop Atom (Seq Atom Atom) (XOR Atom (Seq Atom Atom)))))) Atom)

---------------------------------------------------
-- Some arbitrarily generated safe workflow nets --
---------------------------------------------------

namedArbitraryNets :: [(SafeWorkflow, [Char])]
namedArbitraryNets = zipWith (\net id -> (net, "arbitrary-" <> show id)) arbitraryNets [0..]

arbitraryNets :: [SafeWorkflow]
arbitraryNets =
  [ Atom
  , XOR {xorLhs = AND {andBranches = Atom :| [Atom]}, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}
  , Seq {seqLhs = GenLoop {gLoopIn = Nothing, gLoopExit = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}, gLoopOut = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}}, seqRhs = XOR {xorLhs = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}, xorRhs = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}}}
  , Seq {seqLhs = GenLoop {gLoopIn = Nothing, gLoopExit = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, gLoopOut = AND {andBranches = Atom :| [Atom]}}, seqRhs = AND {andBranches = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom} :| [GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Nothing, gXorMToLhs = Nothing},GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom}]}}
  , GenLoop {gLoopIn = Nothing, gLoopExit = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Nothing}, gLoopOut = GenLoop {gLoopIn = Nothing, gLoopExit = AND {andBranches = Atom :| [Atom,Atom,Atom]}, gLoopOut = GenLoop {gLoopIn = Nothing, gLoopExit = AND {andBranches = Atom :| [Atom,Atom]}, gLoopOut = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom}}}}
  , AND {andBranches = Seq {seqLhs = Seq {seqLhs = Atom, seqRhs = Atom}, seqRhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}} :| [GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom},XOR {xorLhs = AND {andBranches = Atom :| [Atom]}, xorRhs = XOR {xorLhs = Seq {seqLhs = Atom, seqRhs = Atom}, xorRhs = Atom}}]}
  , AND {andBranches = XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = GenLoop {gLoopIn = Just (GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}), gLoopExit = XOR {xorLhs = Atom, xorRhs = Atom}, gLoopOut = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}}} :| [XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = AND {andBranches = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}} :| [XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}]}}]}
  , Seq {seqLhs = GenLoop {gLoopIn = Nothing, gLoopExit = GenLoop {gLoopIn = Just (AND {andBranches = Atom :| [Atom]}), gLoopExit = Atom, gLoopOut = Seq {seqLhs = Atom, seqRhs = Atom}}, gLoopOut = XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = Atom}}, seqRhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, xorRhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}}}}
  , XOR {xorLhs = GenLoop {gLoopIn = Nothing, gLoopExit = XOR {xorLhs = AND {andBranches = Seq {seqLhs = Atom, seqRhs = Atom} :| [Seq {seqLhs = Atom, seqRhs = Atom}]}, xorRhs = Seq {seqLhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, seqRhs = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom}}}, gLoopOut = AND {andBranches = XOR {xorLhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, xorRhs = Atom} :| [Seq {seqLhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, seqRhs = AND {andBranches = Atom :| [Atom,Atom]}}]}}, xorRhs = XOR {xorLhs = Seq {seqLhs = Atom, seqRhs = AND {andBranches = Atom :| [Atom,Atom,Atom]}}, xorRhs = XOR {xorLhs = AND {andBranches = Atom :| [Atom,Atom,Atom,Atom]}, xorRhs = XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}}}}}
  , AND {andBranches = AND {andBranches = XOR {xorLhs = AND {andBranches = Atom :| [Atom]}, xorRhs = Atom} :| [XOR {xorLhs = Seq {seqLhs = AND {andBranches = Atom :| [Atom]}, seqRhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}}, xorRhs = Atom}]} :| [XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = GenLoop {gLoopIn = Just (GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Nothing}), gLoopExit = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, gLoopOut = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}}}]}
  ]

namedCrossValidWitnessNets :: [(ExtendedFCSW, [Char])]
namedCrossValidWitnessNets = zip crossValidWitnessNets
  [ "local loop inside AND branch"
  , "looping AND branches"
  , "local backward jump to dead-end state"
  , "incorrect direct backward jump to AND branch"
  , "incorrect direct backward jump to before AND-split"
  , "incorrect direct forward jump to AND branches"
  ]

-- \ For these workflows the split-and-merge alogrithm should give the same result
-- as the general soundness checking algorithm.
crossValidWitnessNets :: [ExtendedFCSW]
crossValidWitnessNets =
  [ -- local loop inside AND branch
    EFCSW $ ExtendedSW
      (Seq (AND2 Atom Atom) Atom) $
      S.fromList
        [ Arrow (makeWorkflowState [Name "3"]) (makeWorkflowState [Name "5"])
        , Arrow (makeWorkflowState [Name "5"]) (makeWorkflowState [Name "5"])
        ]

  -- looping AND branches
  , EFCSW $ ExtendedSW
      Atom $
      S.fromList
        [ Arrow startState (makeWorkflowState [Name "1", Name "2"])
        , Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "2"])
        , Arrow (makeWorkflowState [Name "2"]) (makeWorkflowState [Name "2"])
        ]

  -- local backward jump to dead-end state
  , EFCSW $ ExtendedSW
      (Seq (AND2 Atom Atom) Atom) $
      S.fromList
        [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3"])
        ]
  -- incorrect direct backward jump to AND branch
  , EFCSW $ ExtendedSW
      (Seq (AND2 Atom Atom) Atom) $
      S.fromList
        [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "2", Name "3"])
        ]
  -- incorrect direct backward jump to before AND-split
  , EFCSW $ ExtendedSW
      (Seq (AND2 Atom Atom) Atom) $
      S.fromList
        [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3"] `wfUnion` startState)
        ]

  -- correct direct forward jump to AND branches
  , EFCSW $ ExtendedSW
      (AND2 Atom Atom) $
      S.fromList
        [ Arrow startState (makeWorkflowState [Name "1", Name "2"])
        ]
  ]

-- | These workflows are sound but should be rejected by the split-and-merge analysis
-- (they are not safe workflows).
namedSoundButNotSafeWitnessNets :: [(ExtendedFCSW, [Char])]
namedSoundButNotSafeWitnessNets = zip soundButNotSafeWitnessNets
  [ "all transitions are direct"
  , "some transitions are direct"
  , "correct direct backward jump to AND-join"
  , "correct direct backward jump to AND branches"
  , "correct indirect backward jump to AND-join"
  , "correct indirect backward jump to AND branches"
  , "correct direct forward jump into AND-join"
  , "correct direct forward jump to AND branches"
  ]

soundButNotSafeWitnessNets :: [ExtendedFCSW]
soundButNotSafeWitnessNets =
  [ -- all transitions are direct
    EFCSW $ ExtendedSW
      Atom $
      S.fromList
        [ Arrow startState (makeWorkflowState [Name "a", Name "b"])
        , Arrow (makeWorkflowState [Name "a", Name "b"]) endState
        ]

  -- some transitions are direct
  , EFCSW $ ExtendedSW
      Atom $
      S.fromList
        [ Arrow startState (makeWorkflowState [Name "a1", Name "b"])
        , Arrow (makeWorkflowState [Name "a1"]) (makeWorkflowState [Name "a2"])
        , Arrow (makeWorkflowState [Name "a2", Name "b"]) endState
        ]

  -- correct direct backward jump to AND-join
  , EFCSW $ ExtendedSW
      (Seq (AND2 Atom Atom) Atom) $
      S.fromList
        [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3", Name "5"])
        ]

  -- correct direct backward jump to AND branches
  , EFCSW $ ExtendedSW
      (Seq (AND2 Atom Atom) Atom) $
      S.fromList
        [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3", Name "4"])
        ]

  -- correct indirect backward jump to AND-join
  , EFCSW $ ExtendedSW
      (Seq (AND2 Atom Atom) Atom) $
      S.fromList
        [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "6", Name "7"])
        , Arrow (makeWorkflowState [Name "6"]) (makeWorkflowState [Name "3"])
        , Arrow (makeWorkflowState [Name "7"]) (makeWorkflowState [Name "5"])
        ]

  -- correct indirect backward jump to AND branches
  , EFCSW $ ExtendedSW
      (Seq (AND2 Atom Atom) Atom) $
      S.fromList
        [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "6", Name "7"])
        , Arrow (makeWorkflowState [Name "6"]) (makeWorkflowState [Name "3"])
        , Arrow (makeWorkflowState [Name "7"]) (makeWorkflowState [Name "4"])
        ]

  -- correct direct forward jump to AND-join
  , EFCSW $ ExtendedSW
      (AND2 Atom Atom) $
      S.fromList
        [ Arrow startState (makeWorkflowState [Name "2", Name "4"])
        ]

  -- correct direct forward jump to AND branches
  , EFCSW $ ExtendedSW
      (AND2 Atom Atom) $
      S.fromList
        [ Arrow startState (makeWorkflowState [Name "2", Name "3"])
        ]
  ]
