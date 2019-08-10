{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Workflow.SafeWorkflow
  ( SafeWorkflow(Atom, AND, GenLoop)

  , pattern XOR
  , pattern XOR3
  , pattern GenXOR
  , pattern AND2
  , pattern Seq
  , pattern SimpleLoop
  , pattern Loop

  , xorLhs
  , xorRhs
  , gXorLhsIn
  , gXorLhsOut
  , gXorRhsIn
  , gXorRhsOut
  , gXorLhsToRhs
  , seqLhs
  , seqRhs

  , constructTransitions

  , GACFArrow(..)
  , GACFPlace(..)
  ) where

import Protolude

import Data.Maybe (fromJust)
import Data.Set (Set(..))
import Data.Map (Map(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (nub)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import Control.Monad.Gen
import Control.Monad.Writer

import Test.QuickCheck hiding (Gen)
import qualified Test.QuickCheck as QC

import Language.FCL.AST
import Language.FCL.Orphans()

-- NOTE: can't return back to different places from a loop (eg.: novation.s)
-- NOTE: syncronisation points are always singleton states (places) (eg.: novation.s)
-- NOTE: safe workflows are always progressive, you can't jump into a loop (loops are isolated)
--       GenXOR allowed this, but GenACF does not

-- | An ordered place in a general acyclic control-flow.
data GACFPlace = Entry  -- ^ Entry point to the general acyclic control-flow. Precedes every other place.
               | P Int  -- ^ Labelled intermediate place. Its ordering is defined by its integer label.
               | Exit   -- ^ Exit point from the general acyclic control-flow. Succeeds every other place.
  deriving (Eq, Ord, Show, Generic, NFData, Arbitrary) -- TODO: remove Arbitrary

data GACFArrow = GACFArrow GACFPlace GACFPlace
  deriving (Eq, Ord, Show, Generic, NFData, Arbitrary) -- TODO: remove Arbitrary

-- | Workflow nets that are sound by construction. We only allow these _safe_ workflow nets
-- to be constructed in very specific ways in order to make soundness verification automatic.
data SafeWorkflow
  -- TODO: should be List2 instead
  -- TODO: branc in the comment
  -- | AND splitting into multiple workflows.
  = AND { andBranches :: NonEmpty SafeWorkflow    -- ^ At least one branc
        }
  -- | Looping construct with option to exit from both the body of the loop and the head of the loop.
  -- If the first half of the body is empty, we exit from head, otherwise exit from the body.
  | GenLoop { gLoopIn   :: Maybe SafeWorkflow     -- ^ First half of the body of the loop
            , gLoopExit :: SafeWorkflow           -- ^ Exit from the loop
            , gLoopOut  :: SafeWorkflow           -- ^ Seconds half of the body of the loop
            }
  -- | Generalized acyclic control-flow.
  | GenACF' { gACFMap :: Map GACFArrow [SafeWorkflow]
            }
  -- | Atom representing a single transition.
  | Atom
  deriving (Eq, Ord, Show, Generic, NFData)

-- TODO: redefine these using record pattern synonyms: https://gitlab.haskell.org/ghc/ghc/wikis/pattern-synonyms/record-pattern-synonyms

-- | XOR with two branches.
pattern XOR :: SafeWorkflow -> SafeWorkflow -> SafeWorkflow
pattern XOR lhs rhs <- GenACF' (M.toList -> [(GACFArrow Entry Exit, [lhs, rhs])])
  where XOR lhs rhs  = GenACF' (M.fromList  [(GACFArrow Entry Exit, [lhs, rhs])])

-- | XOR with three branches.
pattern XOR3 :: SafeWorkflow -> SafeWorkflow -> SafeWorkflow -> SafeWorkflow
pattern XOR3 a b c   = XOR a (XOR b c)

-- | XOR with unidirectional communication between branches.
pattern GenXOR :: SafeWorkflow -> SafeWorkflow -> SafeWorkflow -> SafeWorkflow -> SafeWorkflow -> SafeWorkflow
pattern GenXOR lhsIn lhsOut rhsIn rhsOut lhsToRhs <- GenACF' (M.toList -> [(GACFArrow Entry (P 1), [lhsIn]), (GACFArrow (P 1) Exit, [lhsOut]), (GACFArrow Entry (P 2), [rhsIn]), (GACFArrow (P 2) Exit, [rhsOut]), (GACFArrow (P 1) (P 2), [lhsToRhs])])
  where GenXOR lhsIn lhsOut rhsIn rhsOut lhsToRhs =  GenACF' (M.fromList  [(GACFArrow Entry (P 1), [lhsIn]), (GACFArrow (P 1) Exit, [lhsOut]), (GACFArrow Entry (P 2), [rhsIn]), (GACFArrow (P 2) Exit, [rhsOut]), (GACFArrow (P 1) (P 2), [lhsToRhs])])

-- | Sequencing two safe workflows.
pattern Seq :: SafeWorkflow -> SafeWorkflow -> SafeWorkflow
pattern Seq lhs rhs <- GenACF' (M.toList -> [(GACFArrow Entry (P 1), [lhs]), (GACFArrow (P 1) Exit, [rhs])])
  where Seq lhs rhs =  GenACF' (M.fromList  [(GACFArrow Entry (P 1), [lhs]), (GACFArrow (P 1) Exit, [rhs])])

-- | Unidirectional pattern general acyclic control-flow.
pattern GenACF :: Map GACFArrow [SafeWorkflow] -> SafeWorkflow
pattern GenACF m <- GenACF' m

-- | AND with two branches.
pattern AND2 :: SafeWorkflow -> SafeWorkflow -> SafeWorkflow
pattern AND2 lhs rhs = AND (lhs :| [rhs])

-- | Simple loop with exit only from the head.
pattern SimpleLoop :: SafeWorkflow -> SafeWorkflow -> SafeWorkflow
pattern SimpleLoop loop exit = GenLoop Nothing exit loop

-- | Loop with exit from the body.
pattern Loop :: SafeWorkflow -> SafeWorkflow -> SafeWorkflow -> SafeWorkflow
pattern Loop loopIn exit loopOut = GenLoop (Just loopIn) exit loopOut

xorLhs :: SafeWorkflow -> SafeWorkflow
xorLhs (XOR lhs _) = lhs

xorRhs :: SafeWorkflow -> SafeWorkflow
xorRhs (XOR _ rhs) = rhs

gXorLhsIn :: SafeWorkflow -> SafeWorkflow
gXorLhsIn (GenXOR lhsIn _ _ _ _) = lhsIn

gXorLhsOut :: SafeWorkflow -> SafeWorkflow
gXorLhsOut (GenXOR _ lhsOut _ _ _) = lhsOut

gXorRhsIn :: SafeWorkflow -> SafeWorkflow
gXorRhsIn (GenXOR _ _ rhsIn _ _) = rhsIn

gXorRhsOut :: SafeWorkflow -> SafeWorkflow
gXorRhsOut (GenXOR _ _ _ rhsOut _) = rhsOut

gXorLhsToRhs :: SafeWorkflow -> SafeWorkflow
gXorLhsToRhs (GenXOR _ _ _ _ lhsToRhs) = lhsToRhs

seqLhs :: SafeWorkflow -> SafeWorkflow
seqLhs (Seq lhs _) = lhs

seqRhs :: SafeWorkflow -> SafeWorkflow
seqRhs (Seq _ rhs) = rhs

isIntermediate :: GACFPlace -> Bool
isIntermediate (P _) = True
isIntermediate _     = False

-- | Collects all the invalid arrows in a general acyclic control-flow (non-recursive).
-- An arrow is invalid if it loops back to the same place,
-- or goes back to a place preceeding the source place.
invalidArrows :: [GACFArrow] -> [GACFArrow]
invalidArrows acfArrows = filter isInvalid acfArrows where
  isInvalid :: GACFArrow -> Bool
  isInvalid (GACFArrow from to) = from >= to

mkGenACF :: Map GACFArrow [SafeWorkflow] -> Either [GACFArrow] SafeWorkflow
mkGenACF acfMap = case invalidArrows acfArrows of
  [] -> Right $ GenACF' acfMap
  xs -> Left xs
  where
    acfArrows :: [GACFArrow]
    acfArrows = map fst . M.toList $ acfMap

unsafeMkGenACF :: Map GACFArrow [SafeWorkflow] -> SafeWorkflow
unsafeMkGenACF = either mkError identity . mkGenACF where
  mkError arrows = panic $ "unsafeMkGenACF: The following arrows are invalid: " <> show arrows

-- | Make a workflow state from a `Name`.
singletonWfState :: Name -> WorkflowState
singletonWfState = unsafeWorkflowState . S.singleton . makePlace

-- | Generate a unique name.
genName :: (MonadGen e m, Show e) => m Name
genName = Name . show <$> gen

-- | Generate a uniq workflow state.
genWfState :: (MonadGen e m, Show e) => m WorkflowState
genWfState = singletonWfState <$> genName

-- | Construct the list of transitions from a given `SafeWorkflow`.
constructTransitions :: SafeWorkflow -> [Transition]
constructTransitions = runGen . execWriterT . constructTransitionsM startState endState

-- | Construct the list of transitions from a given `SafeWorkflow` `swf` and `start` and `end` states.
-- The open-ended transitions of `swf` will be connected to the `start` and `end` states
-- based on the stucture of `swf`.
constructTransitionsM :: WorkflowState -> WorkflowState -> SafeWorkflow -> (WriterT [Transition] (Gen Integer)) ()
constructTransitionsM start end (AND branches) = do
  inOuts <- forM (NE.toList branches) $ \br -> do
    inSt  <- genWfState
    outSt <- genWfState
    constructTransitionsM inSt outSt br
    pure (inSt, outSt)
  let (ins, outs) = unzip inOuts
  tell [Arrow start (mconcat ins), Arrow (mconcat outs) end]
constructTransitionsM start end (Seq lhs rhs) = do
  inBetween <- genWfState
  constructTransitionsM start inBetween lhs
  constructTransitionsM inBetween end   rhs
constructTransitionsM start end (SimpleLoop loop exit) = do
  constructTransitionsM start start loop
  constructTransitionsM start end   exit
constructTransitionsM start end (Loop gLoopIn exit gLoopOut) = do
  inBetween <- genWfState
  constructTransitionsM start inBetween gLoopIn
  constructTransitionsM inBetween end   exit
  constructTransitionsM inBetween start gLoopOut
constructTransitionsM start end (GenACF (M.toList -> acfList)) = do
  let acfArrows = map fst acfList
      acfPlaces = S.fromList
                . filter isIntermediate
                . concatMap (\(GACFArrow x y) -> [x,y])
                $ acfArrows
  stateMap <- sequence $ M.fromSet (const genWfState) acfPlaces
  let getState :: GACFPlace -> WorkflowState
      getState Entry = start
      getState Exit  = end
      getState p     = fromMaybe (panic $ "constructTransitionsM: Place " <> show p <> " is not present in state map.") $ M.lookup p stateMap
  forM_ acfList $ \(GACFArrow from to, swfs) -> do
    let from' = getState from
        to'   = getState to
    mapM_ (constructTransitionsM from' to') swfs
constructTransitionsM start end Atom = do
  tell [Arrow start end]

-- TODO: add genACF generation
instance Arbitrary SafeWorkflow where
  arbitrary = sized genSWFNet where
    -- | Sized `SafeWorkflow generation
    genSWFNet :: Int -> QC.Gen SafeWorkflow
    genSWFNet n |          n <= 1 = atom
    genSWFNet n | 1 < n && n <= 2 = maxComplexity 2
    genSWFNet n | 2 < n && n <= 3 = maxComplexity 3
    genSWFNet n | 3 < n && n <= 4 = maxComplexity 4
    genSWFNet n | 4 < n           = maxComplexity 5
    genSWFNet n = panic $ "Negative value for SafeWorkflow generation: " <> show n

    partitionThen :: Int -> Int -> ([Int] -> QC.Gen a) -> QC.Gen a
    partitionThen n k g = do
      let partitions = filter ((==k) . length) $ partitionInt n
      case partitions of
        [] -> panic $ "partitionThen: Couldn't partition " <> show n <> " into " <> show k <> " integers"
        _  -> do
          aPartition <- elements partitions
          g aPartition

    maxComplexity :: Int -> QC.Gen SafeWorkflow
    maxComplexity n = oneof $ take n (allComplexities n)

    allComplexities :: Int -> [QC.Gen SafeWorkflow]
    allComplexities n = [ atom, complexity2 n, complexity3 n, complexity4 n, complexity5 n ]

    atom :: QC.Gen SafeWorkflow
    atom = pure Atom

    complexity2 :: Int -> QC.Gen SafeWorkflow
    complexity2 n = partitionThen n 2 $ \partition -> do
      case partition of
        [k1,k2] -> oneof
          [ XOR <$> genSWFNet k1 <*> genSWFNet k2
          , Seq <$> genSWFNet k1 <*> genSWFNet k2
          , SimpleLoop <$> genSWFNet k1 <*> genSWFNet k2
          ]
        _ -> panic $ show n <> " was not partitioned into 2 components"

    complexity3 :: Int -> QC.Gen SafeWorkflow
    complexity3 n = partitionThen n 3 $ \partition -> do
      case partition of
        [k1,k2,k3] -> oneof
          [ Loop <$> genSWFNet k1 <*> genSWFNet k2 <*> genSWFNet k3
          , XOR3 <$> genSWFNet k1 <*> genSWFNet k2 <*> genSWFNet k3
          ]
        _ -> panic $ show n <> " was not partitioned into 3 components"

    complexity4 :: Int -> QC.Gen SafeWorkflow
    complexity4 n = partitionThen n 4 $ \partition -> do
      case partition of
        [k1,k2,k3,k4] -> oneof
          [ AND <$> someSWFNets (n-2) 2
          ]
        _ -> panic $ show n <> " was not partitioned into 4 components"

    complexity5 :: Int -> QC.Gen SafeWorkflow
    complexity5 n = partitionThen n 5 $ \partition -> do
      case partition of
        [k1,k2,k3,k4,k5] -> oneof
          [ AND <$> someSWFNets (n-2) 3
          , GenXOR <$> genSWFNet k1
                   <*> genSWFNet k2
                   <*> genSWFNet k3
                   <*> genSWFNet k4
                   <*> genSWFNet k5
          ]
        _ -> panic $ show n <> " was not partitioned into 5 components"

    -- | Generates `SafeWorkflow`s of summed size `n`.
    someSWFNets :: Int -> Int -> QC.Gen (NonEmpty SafeWorkflow)
    someSWFNets n k =
      partitionThen n k $ \ps -> do
        xs <- mapM genSWFNet ps
        pure $ NE.fromList xs

    partitionInt d = go d d where
      go _  0  = [[]]
      go !h !n = [ a:as | a<-[1..min n h], as <- go a (n-a) ]

  shrink x@AND{..} = Atom : genericShrink x
  shrink x = genericShrink x
