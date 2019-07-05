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
