{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Workflow.SafeWorkflow.Examples
  ( namedBasicNets
  , namedExampleNets
  , namedArbitraryNets
  , namedCrossValidWitnessNets
  , namedSoundButNotSafeWitnessNets
  ) where

import Protolude

import Data.List.List2 (List2(..))

import qualified Data.List.List2 as L2
import qualified Data.Set as S
import qualified Data.Map as M

import Language.FCL.AST (Name(..), Place(..), WorkflowState(..), Transition(..), makeWorkflowState, startState, endState, wfUnion)
import Language.FCL.SafeWorkflow
  ( SafeWorkflow(..)
  , pattern XOR
  , pattern Seq
  , pattern SimpleLoop
  , pattern Loop
  , unsafeMkACF
  , ACFArrow(..)
  , ACFPlace(..)

  , pattern AND2
  , pattern XOR3
  , pattern GenXOR
  )

import Test.Workflow.SafeWorkflow.Extended

-------------------------------------------------
-- Basic building blocks of safe workflow nets --
-------------------------------------------------

namedBasicNets :: [([Char], SafeWorkflow)]
namedBasicNets =
  [ ( "atom"
    , Atom
    )
  , ( "XOR"
    , XOR Atom (XOR Atom Atom)
    )
  , ( "AND"
    , AND [Atom, Atom, Atom]
    )
  , ( "Seq"
    , Seq Atom Atom
    )
  , ( "SimpleLoop"
    , SimpleLoop Atom Atom
    )
  , ( "Loop"
    , Loop Atom Atom Atom
    )
  , ( "toRightGenXOR"
    , GenXOR Atom Atom Atom Atom Atom
    )
  , ( "toLeftGenXOR"
    , GenXOR Atom Atom Atom Atom Atom
    )
  ]

-------------------------------------------------------------------
-- Already existing examples reproduced using safe workflow nets --
-------------------------------------------------------------------

namedExampleNets :: [([Char], SafeWorkflow)]
namedExampleNets =
  [ ( "swap"
    ,  XOR Atom (Loop Atom Atom (XOR Atom Atom))
    )
  , ( "concurrent"
    ,  AND [Atom, Atom]
    )
  , ( "amendment"
    ,  Seq (AND [Atom, Atom]) (SimpleLoop (XOR (Seq Atom Atom) Atom) Atom)
    )
  , ( "graph"
    ,  Seq (XOR (Seq Atom Atom) (Seq Atom Atom)) Atom
    )
  , ( "novation" -- not exactly the same, but isomorphic
    ,  Seq (AND2 Atom Atom) (Loop (AND2 Atom Atom) Atom (AND2 Atom Atom))
    )
  , ( "loan-contract"
    ,  Seq Atom (Loop Atom (XOR (Seq Atom (SimpleLoop Atom Atom)) Atom) Atom)
    )
  , ( "zcb"
    ,  XOR Atom (Loop Atom (Seq Atom Atom) (XOR Atom Atom))
    )
  , ( "product"
    , Seq Atom (XOR (Seq Atom (SimpleLoop (XOR3 Atom Atom Atom) (XOR (Seq Atom (XOR3 (Seq Atom Atom) (Seq Atom Atom) (Seq Atom Atom))) (Loop Atom (Seq Atom Atom) (XOR Atom (Seq Atom Atom)))))) Atom)
    )
  , ( "gas-forward-simple"
    ,  Seq Atom (XOR (GenXOR Atom (SimpleLoop Atom Atom) Atom Atom Atom) Atom)
    )
  , ( "gas-forward"
    , let gasForwardNominationSubNet = SimpleLoop Atom (Seq (AND2 Atom (SimpleLoop Atom Atom)) (SimpleLoop Atom Atom)) in
        Seq Atom (XOR (GenXOR Atom (SimpleLoop Atom Atom) Atom gasForwardNominationSubNet Atom) Atom)
    )
  ]

---------------------------------------------------
-- Some arbitrarily generated safe workflow nets --
---------------------------------------------------

-- | Just some arbitrarily generated examples.
namedArbitraryNets :: [([Char], SafeWorkflow)]
namedArbitraryNets = zipWith (\id net -> ("arbitrary-" <> show id, net)) [0..]
  [ Atom
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 1),Atom :| []),(ACFArrow (P 1) Exit,Atom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry Exit,Atom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 7391920812667194482),Atom :| []),(ACFArrow Entry Exit,Atom :| []),(ACFArrow (P 7391920812667194482) Exit,Atom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),AND {andBranches = List2 Atom Atom []} :| []),(ACFArrow (P 10000000000) (P 20000000000),Atom :| []),(ACFArrow (P 20000000000) (P 30000000000),Atom :| []),(ACFArrow (P 30000000000) (P 40000000000),Atom :| []),(ACFArrow (P 40000000000) (P 50000000000),Atom :| []),(ACFArrow (P 50000000000) Exit,Atom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry Exit,Atom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),Atom :| []),(ACFArrow (P 10000000000) (P 20000000000),Atom :| []),(ACFArrow (P 20000000000) Exit,Atom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),Atom :| []),(ACFArrow Entry (P 20000000000),Atom :| []),(ACFArrow Entry Exit,Atom :| []),(ACFArrow (P 10000000000) (P 20000000000),GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom} :| []),(ACFArrow (P 20000000000) Exit,Atom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),AND {andBranches = List2 Atom Atom []} :| []),(ACFArrow Entry Exit,Atom :| []),(ACFArrow (P 10000000000) (P 10093128181),Atom :| []),(ACFArrow (P 10000000000) (P 20000000000),GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom} :| []),(ACFArrow (P 10093128181) (P 30000000000),Atom :| []),(ACFArrow (P 20000000000) (P 30000000000),GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom} :| []),(ACFArrow (P 30000000000) Exit,Atom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry Exit,Atom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),Atom :| []),(ACFArrow Entry (P 20000000000),Atom :| []),(ACFArrow (P 10000000000) (P 20000000000),Atom :| []),(ACFArrow (P 10000000000) Exit,Atom :| []),(ACFArrow (P 20000000000) Exit,Atom :| [])]
  ]

-- | Cases generated by QuickCheck that revealed bugs in the Split-and-Merge
-- prototype
namedCrossValidWitnessNets :: [([Char], ExtendedFCSW)]
namedCrossValidWitnessNets =
  [ ( "local loop inside AND branch"
    , EFCSW $ ExtendedSW
        (Seq (AND2 Atom Atom) Atom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "3"]) (makeWorkflowState [Name "5"])
          , Arrow (makeWorkflowState [Name "5"]) (makeWorkflowState [Name "5"])
          ]
    )
  , ( "looping AND branches"
    , EFCSW $ ExtendedSW
        Atom $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "1", Name "2"])
          , Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "2"])
          , Arrow (makeWorkflowState [Name "2"]) (makeWorkflowState [Name "2"])
          ]
    )
  , ( "local backward jump to dead-end state"
    , EFCSW $ ExtendedSW
        (Seq (AND2 Atom Atom) Atom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3"])
          ]
    )
  , ( "incorrect direct backward jump to AND branch"
    , EFCSW $ ExtendedSW
        (Seq (AND2 Atom Atom) Atom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "2", Name "3"])
          ]
    )
  , ( "incorrect direct backward jump to before AND-split"
    , EFCSW $ ExtendedSW
        (Seq (AND2 Atom Atom) Atom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3"] `wfUnion` startState)
          ]
    )
  , ( "incorrect direct forward jump to AND branches"
    , EFCSW $ ExtendedSW
        (AND2 Atom Atom) $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "1", Name "2"])
          ]
    )
  ]

-- | These workflows are sound but should be rejected by the split-and-merge analysis
-- (they are not safe workflows).
namedSoundButNotSafeWitnessNets :: [([Char], ExtendedFCSW)]
namedSoundButNotSafeWitnessNets =
  [ ( "all transitions are direct"
    , EFCSW $ ExtendedSW
        Atom $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "a", Name "b"])
          , Arrow (makeWorkflowState [Name "a", Name "b"]) endState
          ]
    )
  , ( "some transitions are direct"
    , EFCSW $ ExtendedSW
        Atom $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "a1", Name "b"])
          , Arrow (makeWorkflowState [Name "a1"]) (makeWorkflowState [Name "a2"])
          , Arrow (makeWorkflowState [Name "a2", Name "b"]) endState
          ]
    )
  , ( "correct direct backward jump to AND-join"
    , EFCSW $ ExtendedSW
        (Seq (AND2 Atom Atom) Atom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3", Name "5"])
          ]
    )
  , ( "correct direct backward jump to AND branches"
    , EFCSW $ ExtendedSW
        (Seq (AND2 Atom Atom) Atom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3", Name "4"])
          ]
    )
  , ( "correct indirect backward jump to AND-join"
    , EFCSW $ ExtendedSW
        (Seq (AND2 Atom Atom) Atom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "6", Name "7"])
          , Arrow (makeWorkflowState [Name "6"]) (makeWorkflowState [Name "3"])
          , Arrow (makeWorkflowState [Name "7"]) (makeWorkflowState [Name "5"])
          ]
    )
  , ( "correct indirect backward jump to AND branches"
    , EFCSW $ ExtendedSW
        (Seq (AND2 Atom Atom) Atom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "6", Name "7"])
          , Arrow (makeWorkflowState [Name "6"]) (makeWorkflowState [Name "3"])
          , Arrow (makeWorkflowState [Name "7"]) (makeWorkflowState [Name "4"])
          ]
    )
  , ( "correct direct forward jump into AND-join"
    , EFCSW $ ExtendedSW
        (AND2 Atom Atom) $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "2", Name "4"])
          ]
    )
  , ( "correct direct forward jump to AND branches"
    , EFCSW $ ExtendedSW
        (AND2 Atom Atom) $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "2", Name "3"])
          ]
    )
  ]
