{-# LANGUAGE PatternSynonyms #-}
module Language.FCL.WorkflowGenExamples
  ( module Language.FCL.WorkflowGenExamples
  , module Language.FCL.WorkflowGen
  ) where

import Protolude

import Data.List.NonEmpty (NonEmpty(..))

import Language.FCL.WorkflowGen

namedBasicNets :: [(SafeWorkflowNet, [Char])]
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

basicNets :: [SafeWorkflowNet]
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

basicAtom :: SafeWorkflowNet
basicAtom = Atom

basicXOR :: SafeWorkflowNet
basicXOR = XOR Atom (XOR Atom Atom)

basicAND :: SafeWorkflowNet
basicAND = AND (Atom :| [Atom, Atom])

basicSeq :: SafeWorkflowNet
basicSeq = Seq Atom Atom

basicSimpleLoop :: SafeWorkflowNet
basicSimpleLoop = SimpleLoop Atom Atom

basicLoop :: SafeWorkflowNet
basicLoop = Loop Atom Atom Atom

compeleteGenXOR :: SafeWorkflowNet
compeleteGenXOR = GenXOR Atom Atom Atom Atom (Just Atom) (Just Atom)

toRightGenXOR :: SafeWorkflowNet
toRightGenXOR = GenXOR Atom Atom Atom Atom (Just Atom) Nothing

toLeftGenXOR :: SafeWorkflowNet
toLeftGenXOR = GenXOR Atom Atom Atom Atom Nothing (Just Atom)

simpleGenXOR :: SafeWorkflowNet
simpleGenXOR = GenXOR Atom Atom Atom Atom Nothing Nothing

------------------------

namedExampleNets :: [(SafeWorkflowNet, [Char])]
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

exampleNets :: [SafeWorkflowNet]
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

swapNet :: SafeWorkflowNet
swapNet = XOR Atom (Loop Atom Atom (XOR Atom Atom))

concurrentNet :: SafeWorkflowNet
concurrentNet = AND (Atom :| [Atom])

amendmentNet :: SafeWorkflowNet
amendmentNet = Seq (AND (Atom :| [Atom])) (SimpleLoop (XOR (Seq Atom Atom) Atom) Atom)

graphNet :: SafeWorkflowNet
graphNet = Seq (XOR (Seq Atom Atom) (Seq Atom Atom)) Atom

-- not 1:1, but kind of "isomorphic"
novationNet :: SafeWorkflowNet
novationNet = Seq (AND2 Atom Atom) (Loop (AND2 Atom Atom) Atom (AND2 Atom Atom))

loanContractNet :: SafeWorkflowNet
loanContractNet = Seq Atom (Loop Atom (XOR (Seq Atom (SimpleLoop Atom Atom)) Atom) Atom)

zcbNet :: SafeWorkflowNet
zcbNet = XOR Atom (Loop Atom (Seq Atom Atom) (XOR Atom Atom))

-- can't express with current tools: communication between XOR branches needed (discrepancy -> nomination)
-- we need GenXOR
gasForwardSimpleNet :: SafeWorkflowNet
gasForwardSimpleNet = Seq Atom (XOR (GenXOR Atom (SimpleLoop Atom Atom) Atom Atom (Just Atom) Nothing) Atom)

gasForwardNet :: SafeWorkflowNet
gasForwardNet = Seq Atom (XOR (GenXOR Atom (SimpleLoop Atom Atom) Atom gasForwardNominationSubNet (Just Atom) Nothing) Atom)

gasForwardNominationSubNet :: SafeWorkflowNet
gasForwardNominationSubNet = SimpleLoop Atom (Seq (AND2 Atom (SimpleLoop Atom Atom)) (SimpleLoop Atom Atom))

productNet :: SafeWorkflowNet
productNet = Seq Atom (XOR (Seq Atom (SimpleLoop (XOR3 Atom Atom Atom) (XOR (Seq Atom (XOR3 (Seq Atom Atom) (Seq Atom Atom) (Seq Atom Atom))) (Loop Atom (Seq Atom Atom) (XOR Atom (Seq Atom Atom)))))) Atom)

---------------

-- some arbitrary SafeWorkflowNets

namedArbitraryNets :: [(SafeWorkflowNet, [Char])]
namedArbitraryNets = zipWith (\net id -> (net, "arbitrary-" <> show id)) arbitraryNets [0..]

arbitraryNets :: [SafeWorkflowNet]
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
  -- currently falsifiable
  , AND {andBranches = Atom :| [Seq {seqLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Nothing, gXorMToLhs = Nothing}, seqRhs = AND {andBranches = Atom :| [Atom]}}]}
  ]

------------
