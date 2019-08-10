{-# LANGUAGE PatternSynonyms #-}
module Test.Workflow.SafeWorkflow.Examples
  ( namedBasicNets
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

namedBasicNets :: [([Char], SafeWorkflow)]
namedBasicNets =
  [ ( "atom"
    , Atom
    )
  , ( "XOR"
    , XOR Atom (XOR Atom Atom)
    )
  , ( "AND"
    , AND (Atom :| [Atom, Atom])
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
    ,  AND (Atom :| [Atom])
    )
  , ( "amendment"
    ,  Seq (AND (Atom :| [Atom])) (SimpleLoop (XOR (Seq Atom Atom) Atom) Atom)
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

namedArbitraryNets :: [([Char], SafeWorkflow)]
namedArbitraryNets = zipWith (\id net -> ("arbitrary-" <> show id, net)) [0..] arbitraryNets

arbitraryNets :: [SafeWorkflow]
arbitraryNets =
  [ Atom
  , XOR (AND {andBranches = Atom :| [Atom]}) (XOR Atom Atom)
  -- NOTE: cannot be expressed with GenACF
  -- , Seq {seqLhs = GenLoop {gLoopIn = Nothing, gLoopExit = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}, gLoopOut = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}}, seqRhs = XOR {xorLhs = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}, xorRhs = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}}}
  -- TODO: reenable this
  -- , Seq {seqLhs = GenLoop {gLoopIn = Nothing, gLoopExit = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, gLoopOut = AND {andBranches = Atom :| [Atom]}}, seqRhs = AND {andBranches = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom} :| [XOR (Seq Atom Atom) (Seq Atom Atom), GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom}]}}
  -- TODO: reenable this
  -- , GenLoop {gLoopIn = Nothing, gLoopExit = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorLhsToRhs = Just Atom}, gLoopOut = GenLoop {gLoopIn = Nothing, gLoopExit = AND {andBranches = Atom :| [Atom,Atom,Atom]}, gLoopOut = GenLoop {gLoopIn = Nothing, gLoopExit = AND {andBranches = Atom :| [Atom,Atom]}, gLoopOut = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom}}}}
  -- NOTE: cannot be expressed with GenACF
  -- , AND {andBranches = Seq {seqLhs = Seq {seqLhs = Atom, seqRhs = Atom}, seqRhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}} :| [GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom},XOR {xorLhs = AND {andBranches = Atom :| [Atom]}, xorRhs = XOR {xorLhs = Seq {seqLhs = Atom, seqRhs = Atom}, xorRhs = Atom}}]}
  -- NOTE: cannot be expressed with GenACF
  -- , AND {andBranches = XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = GenLoop {gLoopIn = Just (GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}), gLoopExit = XOR {xorLhs = Atom, xorRhs = Atom}, gLoopOut = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}}} :| [XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = AND {andBranches = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}} :| [XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}]}}]}
  -- NOTE: cannot be expressed with GenACF
  -- , Seq {seqLhs = GenLoop {gLoopIn = Nothing, gLoopExit = GenLoop {gLoopIn = Just (AND {andBranches = Atom :| [Atom]}), gLoopExit = Atom, gLoopOut = Seq {seqLhs = Atom, seqRhs = Atom}}, gLoopOut = XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = Atom}}, seqRhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, xorRhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}}}}
  -- NOTE: cannot be expressed with GenACF
  -- , XOR {xorLhs = GenLoop {gLoopIn = Nothing, gLoopExit = XOR {xorLhs = AND {andBranches = Seq {seqLhs = Atom, seqRhs = Atom} :| [Seq {seqLhs = Atom, seqRhs = Atom}]}, xorRhs = Seq {seqLhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, seqRhs = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom}}}, gLoopOut = AND {andBranches = XOR {xorLhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, xorRhs = Atom} :| [Seq {seqLhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, seqRhs = AND {andBranches = Atom :| [Atom,Atom]}}]}}, xorRhs = XOR {xorLhs = Seq {seqLhs = Atom, seqRhs = AND {andBranches = Atom :| [Atom,Atom,Atom]}}, xorRhs = XOR {xorLhs = AND {andBranches = Atom :| [Atom,Atom,Atom,Atom]}, xorRhs = XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}}}}}
  -- NOTE: cannot be expressed with GenACF
  -- , AND {andBranches = AND {andBranches = XOR {xorLhs = AND {andBranches = Atom :| [Atom]}, xorRhs = Atom} :| [XOR {xorLhs = Seq {seqLhs = AND {andBranches = Atom :| [Atom]}, seqRhs = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}}, xorRhs = Atom}]} :| [XOR {xorLhs = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, xorRhs = GenLoop {gLoopIn = Just (GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorLhsToRhs = Just Atom}), gLoopExit = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}, gLoopOut = XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}}}]}
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
