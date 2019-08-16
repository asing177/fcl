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

  , pattern XOR3
  , pattern GenXOR
  )
import Language.FCL.SafeWorkflow.Simple
  ( SimpleSafeWorkflow
  , pattern SAtom
  , pattern SAND
  , pattern SAND2
  )


import Test.Workflow.SafeWorkflow.Extended

-------------------------------------------------
-- Basic building blocks of safe workflow nets --
-------------------------------------------------

namedBasicNets :: [([Char], SimpleSafeWorkflow)]
namedBasicNets =
  [ ( "atom"
    , SAtom
    )
  , ( "XOR"
    , XOR SAtom (XOR SAtom SAtom)
    )
  , ( "AND"
    , SAND [SAtom, SAtom, SAtom]
    )
  , ( "Seq"
    , Seq SAtom SAtom
    )
  , ( "SimpleLoop"
    , SimpleLoop SAtom SAtom
    )
  , ( "Loop"
    , Loop SAtom SAtom SAtom
    )
  , ( "toRightGenXOR"
    , GenXOR SAtom SAtom SAtom SAtom SAtom
    )
  , ( "toLeftGenXOR"
    , GenXOR SAtom SAtom SAtom SAtom SAtom
    )
  ]

-------------------------------------------------------------------
-- Already existing examples reproduced using safe workflow nets --
-------------------------------------------------------------------

namedExampleNets :: [([Char], SimpleSafeWorkflow)]
namedExampleNets =
  [ ( "swap"
    ,  XOR SAtom (Loop SAtom SAtom (XOR SAtom SAtom))
    )
  , ( "concurrent"
    ,  SAND [SAtom, SAtom]
    )
  , ( "amendment"
    ,  Seq (SAND [SAtom, SAtom]) (SimpleLoop (XOR (Seq SAtom SAtom) SAtom) SAtom)
    )
  , ( "graph"
    ,  Seq (XOR (Seq SAtom SAtom) (Seq SAtom SAtom)) SAtom
    )
  , ( "novation" -- not exactly the same, but isomorphic
    ,  Seq (SAND2 SAtom SAtom) (Loop (SAND2 SAtom SAtom) SAtom (SAND2 SAtom SAtom))
    )
  , ( "loan-contract"
    ,  Seq SAtom (Loop SAtom (XOR (Seq SAtom (SimpleLoop SAtom SAtom)) SAtom) SAtom)
    )
  , ( "zcb"
    ,  XOR SAtom (Loop SAtom (Seq SAtom SAtom) (XOR SAtom SAtom))
    )
  , ( "product"
    , Seq SAtom (XOR (Seq SAtom (SimpleLoop (XOR3 SAtom SAtom SAtom) (XOR (Seq SAtom (XOR3 (Seq SAtom SAtom) (Seq SAtom SAtom) (Seq SAtom SAtom))) (Loop SAtom (Seq SAtom SAtom) (XOR SAtom (Seq SAtom SAtom)))))) SAtom)
    )
  , ( "gas-forward-simple"
    ,  Seq SAtom (XOR (GenXOR SAtom (SimpleLoop SAtom SAtom) SAtom SAtom SAtom) SAtom)
    )
  , ( "gas-forward"
    , let gasForwardNominationSubNet = SimpleLoop SAtom (Seq (SAND2 SAtom (SimpleLoop SAtom SAtom)) (SimpleLoop SAtom SAtom)) in
        Seq SAtom (XOR (GenXOR SAtom (SimpleLoop SAtom SAtom) SAtom gasForwardNominationSubNet SAtom) SAtom)
    )
  ]

---------------------------------------------------
-- Some arbitrarily generated safe workflow nets --
---------------------------------------------------

-- | Just some arbitrarily generated examples.
namedArbitraryNets :: [([Char], SimpleSafeWorkflow)]
namedArbitraryNets = zipWith (\id net -> ("arbitrary-" <> show id, net)) [0..]
  [ SAtom
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 1),SAtom :| []),(ACFArrow (P 1) Exit,SAtom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry Exit,SAtom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 7391920812667194482),SAtom :| []),(ACFArrow Entry Exit,SAtom :| []),(ACFArrow (P 7391920812667194482) Exit,SAtom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),SAND (List2 SAtom SAtom []) :| []),(ACFArrow (P 10000000000) (P 20000000000),SAtom :| []),(ACFArrow (P 20000000000) (P 30000000000),SAtom :| []),(ACFArrow (P 30000000000) (P 40000000000),SAtom :| []),(ACFArrow (P 40000000000) (P 50000000000),SAtom :| []),(ACFArrow (P 50000000000) Exit,SAtom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry Exit,SAtom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),SAtom :| []),(ACFArrow (P 10000000000) (P 20000000000),SAtom :| []),(ACFArrow (P 20000000000) Exit,SAtom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),SAtom :| []),(ACFArrow Entry (P 20000000000),SAtom :| []),(ACFArrow Entry Exit,SAtom :| []),(ACFArrow (P 10000000000) (P 20000000000),GenLoop {gLoopIn = Just SAtom, gLoopExit = SAtom, gLoopOut = SAtom} :| []),(ACFArrow (P 20000000000) Exit,SAtom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),SAND (List2 SAtom SAtom []) :| []),(ACFArrow Entry Exit,SAtom :| []),(ACFArrow (P 10000000000) (P 10093128181),SAtom :| []),(ACFArrow (P 10000000000) (P 20000000000),GenLoop {gLoopIn = Nothing, gLoopExit = SAtom, gLoopOut = SAtom} :| []),(ACFArrow (P 10093128181) (P 30000000000),SAtom :| []),(ACFArrow (P 20000000000) (P 30000000000),GenLoop {gLoopIn = Nothing, gLoopExit = SAtom, gLoopOut = SAtom} :| []),(ACFArrow (P 30000000000) Exit,SAtom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry Exit,SAtom :| [])]
  , unsafeMkACF $ M.fromList [(ACFArrow Entry (P 10000000000),SAtom :| []),(ACFArrow Entry (P 20000000000),SAtom :| []),(ACFArrow (P 10000000000) (P 20000000000),SAtom :| []),(ACFArrow (P 10000000000) Exit,SAtom :| []),(ACFArrow (P 20000000000) Exit,SAtom :| [])]
  ]

-- | Cases generated by QuickCheck that revealed bugs in the Split-and-Merge
-- prototype
namedCrossValidWitnessNets :: [([Char], ExtendedFCSW)]
namedCrossValidWitnessNets =
  [ ( "local loop inside SAND branch"
    , EFCSW $ ExtendedSW
        (Seq (SAND2 SAtom SAtom) SAtom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "3"]) (makeWorkflowState [Name "5"])
          , Arrow (makeWorkflowState [Name "5"]) (makeWorkflowState [Name "5"])
          ]
    )
  , ( "looping SAND branches"
    , EFCSW $ ExtendedSW
        SAtom $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "1", Name "2"])
          , Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "2"])
          , Arrow (makeWorkflowState [Name "2"]) (makeWorkflowState [Name "2"])
          ]
    )
  , ( "local backward jump to dead-end state"
    , EFCSW $ ExtendedSW
        (Seq (SAND2 SAtom SAtom) SAtom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3"])
          ]
    )
  , ( "incorrect direct backward jump to SAND branch"
    , EFCSW $ ExtendedSW
        (Seq (SAND2 SAtom SAtom) SAtom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "2", Name "3"])
          ]
    )
  , ( "incorrect direct backward jump to before SAND-split"
    , EFCSW $ ExtendedSW
        (Seq (SAND2 SAtom SAtom) SAtom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3"] `wfUnion` startState)
          ]
    )
  , ( "incorrect direct forward jump to SAND branches"
    , EFCSW $ ExtendedSW
        (SAND2 SAtom SAtom) $
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
        SAtom $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "a", Name "b"])
          , Arrow (makeWorkflowState [Name "a", Name "b"]) endState
          ]
    )
  , ( "some transitions are direct"
    , EFCSW $ ExtendedSW
        SAtom $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "a1", Name "b"])
          , Arrow (makeWorkflowState [Name "a1"]) (makeWorkflowState [Name "a2"])
          , Arrow (makeWorkflowState [Name "a2", Name "b"]) endState
          ]
    )
  , ( "correct direct backward jump to SAND-join"
    , EFCSW $ ExtendedSW
        (Seq (SAND2 SAtom SAtom) SAtom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3", Name "5"])
          ]
    )
  , ( "correct direct backward jump to SAND branches"
    , EFCSW $ ExtendedSW
        (Seq (SAND2 SAtom SAtom) SAtom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3", Name "4"])
          ]
    )
  , ( "correct indirect backward jump to SAND-join"
    , EFCSW $ ExtendedSW
        (Seq (SAND2 SAtom SAtom) SAtom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "6", Name "7"])
          , Arrow (makeWorkflowState [Name "6"]) (makeWorkflowState [Name "3"])
          , Arrow (makeWorkflowState [Name "7"]) (makeWorkflowState [Name "5"])
          ]
    )
  , ( "correct indirect backward jump to SAND branches"
    , EFCSW $ ExtendedSW
        (Seq (SAND2 SAtom SAtom) SAtom) $
        S.fromList
          [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "6", Name "7"])
          , Arrow (makeWorkflowState [Name "6"]) (makeWorkflowState [Name "3"])
          , Arrow (makeWorkflowState [Name "7"]) (makeWorkflowState [Name "4"])
          ]
    )
  , ( "correct direct forward jump into SAND-join"
    , EFCSW $ ExtendedSW
        (SAND2 SAtom SAtom) $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "2", Name "4"])
          ]
    )
  , ( "correct direct forward jump to SAND branches"
    , EFCSW $ ExtendedSW
        (SAND2 SAtom SAtom) $
        S.fromList
          [ Arrow startState (makeWorkflowState [Name "2", Name "3"])
          ]
    )
  ]
