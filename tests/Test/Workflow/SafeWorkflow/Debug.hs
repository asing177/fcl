{-# LANGUAGE BangPatterns, TypeApplications #-}
module Test.Workflow.SafeWorkflow.Debug where

import Protolude

import qualified Data.Set as S
import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import Language.FCL.AST (Transition(..), Name(..), makeWorkflowState, startState, endState, wfUnion)
import Language.FCL.Analysis (inferTransitions)
import Language.FCL.Parser (parseFile)
import Language.FCL.Pretty (Pretty(..), vsep)
import Language.FCL.Reachability.General (completeReachabilityGraph)
import Language.FCL.Reachability.SplitAndMerge (reachabilityGraph, reverseTransitions, freeChoicePropertyViolations)
import Language.FCL.Reachability.Definitions (WFError(..), ReachabilityGraph, pprReachabilityGraph)

import Test.Workflow.SafeWorkflow
import Test.Workflow.SafeWorkflow.Extended
import Test.Workflow.SafeWorkflow.Examples
import Test.Workflow.SafeWorkflow.Tests

import Prelude ((!!))
import Test.QuickCheck
import Language.FCL.AST (Name(..), Place(..), WorkflowState(..), Transition(..))
import Language.FCL.Graphviz
import Language.FCL.Pretty hiding (print)
import Language.FCL.Debug

import System.Clock

-- witness0 :: ExtendedSW
-- witness0 = ExtendedSW
--   { eswSafeWorkflow = Seq { seqLhs = GenXOR { gXorLhsIn = XOR Atom Atom
--                                             , gXorLhsOut = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}
--                                             , gXorRhsIn = AND {andBranches = Atom :| [Atom]}
--                                             , gXorRhsOut = AND {andBranches = Atom :| [Atom,Atom]}
--                                             , gXorMToRhs = Just (XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}})
--                                             , gXorMToLhs = Just Atom}
--                           , seqRhs = XOR    { xorLhs = Atom
--                                             , xorRhs = XOR  { xorLhs = Seq  { seqLhs = AND {andBranches = Atom :| [Atom]}
--                                                                             , seqRhs = AND {andBranches = Atom :| [Atom,Atom]}
--                                                                             }
--                                                             , xorRhs = AND {andBranches = Atom :| [Atom]}
--                                                             }
--                                             }
--                           }
--   , eswExtraTransitions = S.fromList  [ Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "1"}}]})
--                                               (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "6"}}, Place {placeName = Name {unName = "8"}}]})
--                                       ]
--   }

-- simplifiedWitness0 :: ExtendedSW
-- simplifiedWitness0 = ExtendedSW
--   { eswSafeWorkflow = Seq { seqLhs = GenXOR { gXorLhsIn  = Atom
--                                             , gXorLhsOut = GenLoop {gLoopIn = Just Atom, gLoopExit = Atom, gLoopOut = Atom}
--                                             , gXorRhsIn  = AND {andBranches = Atom :| [Atom]}
--                                             , gXorRhsOut = Atom
--                                             , gXorMToRhs = Just Atom
--                                             , gXorMToLhs = Just Atom
--                                             }
--                           , seqRhs = Atom
--                           }
--   , eswExtraTransitions = S.fromList  [ Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "1"}}]})
--                                               (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "6"}}, Place {placeName = Name {unName = "8"}}]})
--                                       ]
--   }

-- asd :: [ExtendedFCSW]
-- asd =
--   -- FC graph is not smaller (OK) (both take roughly the same amount of time to finish, the the genral algo is faster by a little bit)
--   [ EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = GenXOR {gXorLhsIn = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Nothing, gXorMToLhs = Nothing}, gXorLhsOut = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Nothing}, gXorRhsIn = XOR {xorLhs = Atom, xorRhs = Atom}, gXorRhsOut = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just Atom, gXorMToLhs = Just Atom}, gXorMToRhs = Just (Seq {seqLhs = Atom, seqRhs = Atom}), gXorMToLhs = Nothing}, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "9"}}]}) (WorkflowState {places = S.fromList [PlaceStart,Place {placeName = Name {unName = "1"}},Place {placeName = Name {unName = "2"}}]})]}}
--   -- FC graph is not smaller (OK)
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = AND {andBranches = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom} :| [XOR {xorLhs = Atom, xorRhs = XOR {xorLhs = Atom, xorRhs = Atom}}]}, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "1"}}]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "4"}}]}),Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "3"}}]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "1"}}]})]}}
--   -- FC graph is not smaller (OK)
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Nothing, gXorMToLhs = Just Atom}, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places =S.fromList [Place {placeName = Name {unName = "1"}}]}) (WorkflowState {places =S.fromList [PlaceStart,Place {placeName = Name {unName = "x"}}]})]}}

--   -- FC graph is not smaller
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = XOR {xorLhs = XOR {xorLhs = Atom, xorRhs = Atom}, xorRhs = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom}}, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [PlaceStart]}) (WorkflowState {places = S.fromList [PlaceStart,Place {placeName = Name {unName = "x"}}]}),Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "x"}}]}) (WorkflowState {places = S.fromList [PlaceEnd]})]}}
--   ]

-- qwe :: [ExtendedFCSW]
-- qwe =
--   [ fromSafeWorkflow $ AND {andBranches = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = AND {andBranches = Atom :| [Atom]}} :| [Atom]}
--   , fromSafeWorkflow $ AND {andBranches = XOR {xorLhs = AND {andBranches = Atom :| [Atom]}, xorRhs = GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = Atom}} :| [Atom]}
--   , fromSafeWorkflow $ AND {andBranches = Atom :| [GenLoop {gLoopIn = Nothing, gLoopExit = AND {andBranches = Atom :| [Atom]}, gLoopOut = Atom}]}
--   , fromSafeWorkflow $ GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Nothing, gXorMToLhs = Nothing}}
--   , fromSafeWorkflow $ GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = AND {andBranches = Atom :| []}}
--   , fromSafeWorkflow $ AND {andBranches = Atom :| [GenLoop {gLoopIn = Nothing, gLoopExit = Atom, gLoopOut = AND {andBranches = Atom :| []}}]}
--   -- global loop
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = AND {andBranches = Atom :| [Atom,Atom]}, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "1"}}]}) (WorkflowState {places = S.fromList [PlaceStart]})]}}
--   -- global skip to terminal
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = XOR (Seq Atom Atom) (AND2 Atom Atom), eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "2"}}]}) (WorkflowState {places = S.fromList [PlaceEnd]})]}}
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = XOR (AND2 Atom Atom) (Seq Atom Atom), eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "2"}}]}) (WorkflowState {places = S.fromList [PlaceEnd]})]}}
--   -- global skip to non-terminal
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = XOR (Seq Atom Atom) (AND2 Atom Atom), eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "2"}}]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "1"}}]})]}}
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = XOR (AND2 Atom Atom) (Seq Atom Atom), eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "1"}}]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "5"}}]})]}}
--   , fromSafeWorkflow $ AND {andBranches = GenXOR {gXorLhsIn = Atom, gXorLhsOut = Atom, gXorRhsIn = Atom, gXorRhsOut = Atom, gXorMToRhs = Just (AND {andBranches = Atom :| []}), gXorMToLhs = Just (AND {andBranches = Atom :| [Atom]})} :| [Atom]}

--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = Atom, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [PlaceStart]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "a"}},Place {placeName = Name {unName = "f"}},Place {placeName = Name {unName = "p"}}]}),Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "a"}},Place {placeName = Name {unName = "f"}},Place {placeName = Name {unName = "p"}}]}) (WorkflowState {places = S.fromList [PlaceStart]})]}}
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = Atom, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [PlaceStart]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "a"}},Place {placeName = Name {unName = "f"}},Place {placeName = Name {unName = "p"}}]}),Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "a"}},Place {placeName = Name {unName = "f"}},Place {placeName = Name {unName = "p"}}]}) (WorkflowState {places = S.fromList [PlaceStart]})]}}
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = Atom, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [PlaceStart]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "f"}},Place {placeName = Name {unName = "fz"}}]}),Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "f"}},Place {placeName = Name {unName = "fw"}}]}) (WorkflowState {places = S.fromList [PlaceEnd]}), Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "fz"}}]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "fw"}}]})]}}
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = Atom, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [PlaceStart]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "f"}},Place {placeName = Name {unName = "fw"}}]}),Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "f"}}]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "f"}}]}), Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "fw"}}]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "fw"}}]})]}}

--   -- AND loops back to global state but the workflow is sound
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = AND {andBranches = Atom :| [Atom,Atom]}, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "5"}}]}) (WorkflowState {places = S.fromList [PlaceStart]})]}}
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = AND {andBranches = Atom :| [Atom]},      eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "3"}}]}) (WorkflowState {places = S.fromList [PlaceStart]})]}}

--   -- global skip
--   , EFCSW {fcGetESW = ExtendedSW {eswSafeWorkflow = AND {andBranches = Atom :| [Atom]}, eswExtraTransitions = S.fromList [Arrow (WorkflowState {places = S.fromList [PlaceStart]}) (WorkflowState {places = S.fromList [Place {placeName = Name {unName = "2"}}, Place {placeName = Name {unName = "4"}}]})]}}

--   -- local jump back to dead-end state
--   , EFCSW $ ExtendedSW
--       (Seq (AND2 Atom Atom) Atom) $
--       S.fromList
--         [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3"])

--         ]
--   -- indirect jump back to AND branch
--   , EFCSW $ ExtendedSW
--       (Seq (AND2 Atom Atom) Atom) $
--       S.fromList
--         [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "6", Name "7"])
  --       , Arrow (makeWorkflowState [Name "6"]) (makeWorkflowState [Name "3"])
  --       , Arrow (makeWorkflowState [Name "7"]) (makeWorkflowState [Name "5"])
  --       ]
  -- -- direct jump back to AND branch
  -- , EFCSW $ ExtendedSW
  --     (Seq (AND2 Atom Atom) Atom) $
  --     S.fromList
  --       [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3", Name "5"])
  --       ]
  -- -- incorrect direct jump back to AND branch
  -- , EFCSW $ ExtendedSW
  --     (Seq (AND2 Atom Atom) Atom) $
  --     S.fromList
  --       [ Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "3"] `wfUnion` startState)
  --       ]
  -- ]

-- xcv :: [Set Transition]
-- xcv = map S.fromList
--   [ -- global exit to state with applicable transitions
--     [ Arrow startState (makeWorkflowState [Name "1"])
--     , Arrow (makeWorkflowState [Name "1"]) (makeWorkflowState [Name "2", Name "3"])
--     , Arrow (makeWorkflowState [Name "2", Name "3"]) (makeWorkflowState [Name "2", Name "3"])
--     , Arrow (makeWorkflowState [Name "2"]) startState
--     , Arrow (makeWorkflowState [Name "3"]) endState
--     ]

--   ]

fromSafeWorkflow :: SafeWorkflow -> ExtendedFCSW
fromSafeWorkflow swf = EFCSW $ ExtendedSW swf mempty

eswToTransitions :: ExtendedSW -> [Transition]
eswToTransitions ExtendedSW{..} = (sort . constructTransitions $ eswSafeWorkflow) ++ (S.toList eswExtraTransitions)

printESW :: FilePath -> ExtendedSW -> IO ()
printESW path = workflowWriteSVG path . Transitions . eswToTransitions

printSW :: FilePath -> SafeWorkflow -> IO ()
printSW path = workflowWriteSVG path . Transitions . sort . constructTransitions

corg :: ExtendedSW -> ReachabilityGraph
corg = reverseTransitions . rg

rg :: ExtendedSW -> ReachabilityGraph
rg esw = {- trace (show . pprRG $ (errs, graph) :: [Char]) -} graph where
  (errs, graph) = reachabilityGraph . S.fromList . extendedWorkflowTransitions $ esw

  pprRG :: (Set WFError, ReachabilityGraph) -> Doc
  pprRG (errs, graph) = ppr errs <$$> pprReachabilityGraph graph

crg :: ExtendedSW -> ReachabilityGraph
crg esw = {- trace (show . pprRG $ (errs, graph) :: [Char]) -} graph where
  (errs, graph) = completeReachabilityGraph . S.fromList . extendedWorkflowTransitions $ esw

  pprRG :: (Set WFError, ReachabilityGraph) -> Doc
  pprRG (errs, graph) = ppr errs <$$> pprReachabilityGraph graph

fileRg :: FilePath -> IO ()
fileRg fp = do
  script <- parseFile fp
  let trs = inferTransitions script
      (errs, graph) = reachabilityGraph $ S.fromList trs
  print $ pprReachabilityGraph graph
  print $ ppr errs

onlySafe :: ExtendedSW -> ExtendedSW
onlySafe esw = esw { eswExtraTransitions = mempty }

-- NOTE: could fail
noErrorsFC :: ExtendedFCSW -> Bool
noErrorsFC efcsw
  | esw <- fcGetESW efcsw
  , (freeChoiceErrs, _) <- checkWithBoth esw
  = force freeChoiceErrs `seq` True

-- NOTE: could fail
noErrorsGeneral :: ExtendedFCSW -> Bool
noErrorsGeneral efcsw
  | esw <- fcGetESW efcsw
  , (_, generalErrs) <- checkWithBoth esw
  = force generalErrs `seq` True

testFC :: Int -> IO ()
testFC n = do
  swfs <- generate (vectorOf 1000 $ resize n $ arbitrary :: Gen [SafeWorkflow])
  forM_ swfs $ \swf -> do
    Protolude.print swf
    let sound = null $ fst $ checkWithBoth (fcGetESW $ fromSafeWorkflow swf)
    let sound = isSafeWorkflowSound_SplitAndMerge swf
    Protolude.print sound
    Protolude.print "----------"

-- TODO: delete these
isXOR (XOR _ _) = True
isXOR _ = False

checkSizeIO :: Int -> IO Int
checkSizeIO n = do
  swf <- generate ((resize n $ arbitrary) :: Gen SafeWorkflow)
  return $ length $ constructTransitions swf

printNamedNets :: [(ExtendedFCSW, [Char])] -> IO ()
printNamedNets = mapM_ (\(eswFC, name) -> printESW ("./Extended/" <> name) $ fcGetESW eswFC)


------------------
-- Benchmarking --
------------------

benchmark :: IO ()
benchmark = forM_ [6..20] $ \size -> do
  (splitAndMergeTime, generalTime) <- measure 10000 size
  print $ "size: " <> show size <> "  -  " <> "no. workflows: " <> (show 10000 :: Text)
  print $ ppr $ splitAndMergeTime
  print $ ppr $ generalTime
  print "-----------"

measure :: Int -> Int -> IO (Double, Double)
measure trials size = do
  workflows <- generate (vectorOf trials $ resize size $ arbitrary @SafeWorkflow)
  force workflows `seq` return ()
  start <- getTime Monotonic
  force (map isSafeWorkflowSound_SplitAndMerge workflows) `seq` return ()
  splitAndMergeDone <- getTime Monotonic
  force (map isSafeWorkflowSound_General workflows) `seq` return ()
  generalDone <- getTime Monotonic
  -- print $ ppr $ Sec $ splitAndMergeDone - start
  -- print $ ppr $ Sec $ generalDone - splitAndMergeDone
  return ( toDouble $ splitAndMergeDone - start
         , toDouble $ generalDone - splitAndMergeDone
         )

benchmarkExtended :: IO ()
benchmarkExtended = forM_ [6..20] $ \size -> do
  (splitAndMergeTime, generalTime) <- measureExtended 10000 size
  print $ "size: " <> show size <> "  -  " <> "no. workflows: " <> (show 10000 :: Text)
  print $ ppr $ splitAndMergeTime
  print $ ppr $ generalTime
  print "-----------"

measureExtended :: Int -> Int -> IO (Double, Double)
measureExtended trials size = do
  workflows <- generate (vectorOf trials $ resize size $ arbitrary @ExtendedFCSW)
  force workflows `seq` return ()
  start <- getTime Monotonic
  force (map (fst . checkWithBoth . fcGetESW) workflows) `seq` return ()
  splitAndMergeDone <- getTime Monotonic
  print "SAM DONE"
  force (map (snd . checkWithBoth . fcGetESW) workflows) `seq` return ()
  generalDone <- getTime Monotonic
  print "GEN DONE"
  return ( toDouble $ splitAndMergeDone - start
         , toDouble $ generalDone - splitAndMergeDone
         )

newtype Sec a = Sec a
  deriving (Eq, Ord, Show)

toDouble :: TimeSpec -> Double
toDouble TimeSpec{..} = fromIntegral sec + (fromIntegral nsec / 10^9 :: Double)

instance Pretty (Sec TimeSpec) where
  ppr (Sec t) = ppr $ toDouble t

multiLayerComm :: SafeWorkflow
multiLayerComm = GenACF $ M.fromList
  [ (SWArrow Entry (P 1), [Atom])
  , (SWArrow (P 1) (P 3), [Atom])
  , (SWArrow (P 3) Exit, [Atom])
  , (SWArrow Entry (P 2), [Atom])
  , (SWArrow (P 2) (P 4), [Atom])
  , (SWArrow (P 4) Exit, [Atom])
  , (SWArrow (P 1) (P 2), [Atom])
  , (SWArrow (P 3) (P 4), [Atom])
  ]
