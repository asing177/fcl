{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.FCL.SafeWorkflow
  ( SafeWorkflow(Atom, AND, GenLoop)

  , andBranches
  , gLoopIn
  , gLoopOut
  , gLoopExit

  , mkACF
  , unsafeMkACF

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

  , ACFArrow(..)
  , ACFPlace(..)
  ) where

import Protolude

import qualified GHC.Exts as GHC (IsList(..))

import Data.Set (Set)
import Data.Map (Map)
import Data.List.List2 (List2(..), pattern AsList)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.Gen
import Control.Monad.Writer

import Test.QuickCheck hiding (Gen)
import qualified Test.QuickCheck as QC

import Language.FCL.AST
import Language.FCL.Pretty (ppr, (<+>), Pretty)
import Language.FCL.Orphans()

-- NOTE: can't return back to different places from a loop (eg.: novation.s)
-- NOTE: syncronisation points are always singleton states (places) (eg.: novation.s)
-- NOTE: safe workflows are always progressive, you can't jump into a loop (loops are isolated)
--       GenXOR allowed this, but ACF does not

-- | An ordered place in a general acyclic control-flow.
data ACFPlace = Entry  -- ^ Entry point to the acyclic control-flow. Precedes every other place.
              | P Int  -- ^ Labelled intermediate place. Its ordering is defined by its integer label.
              | Exit   -- ^ Exit point from the acyclic control-flow. Succeeds every other place.
  deriving (Eq, Ord, Show, Generic, NFData)

-- | An arrow between two places in an acyclic control-flow.
-- The first component is always strictly less than the second component.
data ACFArrow = ACFArrow ACFPlace ACFPlace
  deriving (Eq, Ord, Show, Generic, NFData)

-- | A mapping describing the structure of an acyclic control-flow.
-- It maps arrows to a non-empty list of safe workflows. An arrow represents
-- the possible ways to transition from a given state to another one. If the list
-- is not singleton, it represents an XOR-split, multiple ways to exectute the transition.
type ACFMap a = Map ACFArrow (NonEmpty (SafeWorkflow a))

-- | Workflow nets that are sound by construction. We only allow these _safe_ workflow nets
-- to be constructed in very specific ways in order to make soundness verification automatic.
-- The transitions in safe workflows can be annotated by any desired information.
data SafeWorkflow a
  -- | AND splitting into multiple workflows.
  = AND { splitAnnot  :: a                            -- ^ Annotation for the AND-split transition
        , joinAnnot   :: a                            -- ^ Annotation for the AND-join transition
        , andBranches :: List2 (SafeWorkflow a)       -- ^ At least one branch
        }
  -- | Looping construct with option to exit from both the body of the loop and the head of the loop.
  -- If the first half of the body is empty, we exit from head, otherwise exit from the body.
  | GenLoop { gLoopIn   :: Maybe (SafeWorkflow a)     -- ^ First half of the body of the loop
            , gLoopExit :: (SafeWorkflow a)           -- ^ Exit from the loop
            , gLoopOut  :: (SafeWorkflow a)           -- ^ Seconds half of the body of the loop
            }
  -- | Acyclic control-flow.
  | ACF' { acfMap :: ACFMap a
         }
  -- | Atom representing a single transition.
  | Atom { getAnnot :: a                              -- ^ Annotation for the transitions
         }
  deriving (Eq, Ord, Show, Generic, NFData)

-- TODO: redefine these using record pattern synonyms (once they are available)
-- https://gitlab.haskell.org/ghc/ghc/wikis/pattern-synonyms/record-pattern-synonyms

{-# COMPLETE AND, GenLoop,          ACF, Atom #-}
{-# COMPLETE AND, SimpleLoop, Loop, ACF, Atom #-}

-- | XOR with two branches.
pattern XOR :: SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a
pattern XOR lhs rhs <- ACF' (M.toList -> [(ACFArrow Entry Exit, [lhs, rhs])])
  where XOR lhs rhs  = ACF' (M.fromList  [(ACFArrow Entry Exit, [lhs, rhs])])

-- | XOR with three branches.
pattern XOR3 :: SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a
pattern XOR3 a b c   = XOR a (XOR b c)

-- | XOR with unidirectional communication between branches.
pattern GenXOR :: SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a
pattern GenXOR lhsIn lhsOut rhsIn rhsOut lhsToRhs <- ACF' (M.toList -> [(ACFArrow Entry (P 1), [lhsIn]), (ACFArrow (P 1) Exit, [lhsOut]), (ACFArrow Entry (P 2), [rhsIn]), (ACFArrow (P 2) Exit, [rhsOut]), (ACFArrow (P 1) (P 2), [lhsToRhs])])
  where GenXOR lhsIn lhsOut rhsIn rhsOut lhsToRhs =  ACF' (M.fromList  [(ACFArrow Entry (P 1), [lhsIn]), (ACFArrow (P 1) Exit, [lhsOut]), (ACFArrow Entry (P 2), [rhsIn]), (ACFArrow (P 2) Exit, [rhsOut]), (ACFArrow (P 1) (P 2), [lhsToRhs])])

-- | Sequencing two safe workflows.
pattern Seq :: SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a
pattern Seq lhs rhs <- ACF' (M.toList -> [(ACFArrow Entry (P 1), [lhs]), (ACFArrow (P 1) Exit, [rhs])])
  where Seq lhs rhs =  ACF' (M.fromList  [(ACFArrow Entry (P 1), [lhs]), (ACFArrow (P 1) Exit, [rhs])])

-- | Unidirectional pattern general acyclic control-flow.
pattern ACF :: (ACFMap a) -> SafeWorkflow a
pattern ACF m <- ACF' m

-- | AND with two branches.
pattern AND2 :: a -> a -> SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a
pattern AND2 split join lhs rhs <- AND split join (AsList [lhs,rhs])
  where AND2 split join lhs rhs =  AND split join [lhs,rhs]

-- | Simple loop with exit only from the head.
pattern SimpleLoop :: SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a
pattern SimpleLoop loop exit = GenLoop Nothing exit loop

-- | Loop with exit from the body.
pattern Loop :: SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a -> SafeWorkflow a
pattern Loop loopIn exit loopOut = GenLoop (Just loopIn) exit loopOut

noMatchError :: Text -> Text
noMatchError selector = "No match in record selector " <> selector

xorLhs :: SafeWorkflow a -> SafeWorkflow a
xorLhs (XOR lhs _) = lhs
xorLhs _ = panic $ noMatchError "xorLhs"

xorRhs :: SafeWorkflow a -> SafeWorkflow a
xorRhs (XOR _ rhs) = rhs
xorRhs _ = panic $ noMatchError "xorRhs"

gXorLhsIn :: SafeWorkflow a -> SafeWorkflow a
gXorLhsIn (GenXOR lhsIn _ _ _ _) = lhsIn
gXorLhsIn _= panic $ noMatchError "gXorLhsIn"

gXorLhsOut :: SafeWorkflow a -> SafeWorkflow a
gXorLhsOut (GenXOR _ lhsOut _ _ _) = lhsOut
gXorLhsOut _ = panic $ noMatchError "gXorLhsOut"

gXorRhsIn :: SafeWorkflow a -> SafeWorkflow a
gXorRhsIn (GenXOR _ _ rhsIn _ _) = rhsIn
gXorRhsIn _ = panic $ noMatchError "gXorRhsIn"

gXorRhsOut :: SafeWorkflow a -> SafeWorkflow a
gXorRhsOut (GenXOR _ _ _ rhsOut _) = rhsOut
gXorRhsOut _ = panic $ noMatchError "gXorRhsOut"

gXorLhsToRhs :: SafeWorkflow a -> SafeWorkflow a
gXorLhsToRhs (GenXOR _ _ _ _ lhsToRhs) = lhsToRhs
gXorLhsToRhs _ = panic $ noMatchError "gXorLhsToRhs"

seqLhs :: SafeWorkflow a -> SafeWorkflow a
seqLhs (Seq lhs _) = lhs
seqLhs _ = panic $ noMatchError "seqLhs"

seqRhs :: SafeWorkflow a -> SafeWorkflow a
seqRhs (Seq _ rhs) = rhs
seqRhs _ = panic $ noMatchError "seqRhs"

-- | Is an ACF place intermediate (not `Entry` nor `Exit`).
isIntermediate :: ACFPlace -> Bool
isIntermediate (P _) = True
isIntermediate _     = False

-- | Gather the places from an ACF map.
gACFPlaces :: ACFMap a -> Set ACFPlace
gACFPlaces = S.fromList . concatMap (\(ACFArrow x y) -> [x,y]) . M.keys

-- | Collects all the invalid arrows in a general acyclic control-flow (non-recursive).
-- An arrow is invalid if it loops back to the same place,
-- or goes back to a place preceeding the source place.
invalidArrows :: [ACFArrow] -> [ACFArrow]
invalidArrows acfArrows = filter isInvalid acfArrows where
  isInvalid :: ACFArrow -> Bool
  isInvalid (ACFArrow from to) = from >= to

-- | Safe smart constructor for ACFs. Returns a safe workflow
-- if the ACF is correct, or the list of invalid arrows if it is not.
mkACF :: ACFMap a -> Either [ACFArrow] (SafeWorkflow a)
mkACF acfMap = case invalidArrows acfArrows of
  [] -> Right $ ACF' acfMap
  xs -> Left xs
  where
    acfArrows :: [ACFArrow]
    acfArrows = map fst . M.toList $ acfMap

-- | Unsafe smart constructor for ACFs. Returns a safe workflow
-- if the ACF is correct, crashes if it is not.
unsafeMkACF :: ACFMap a -> SafeWorkflow a
unsafeMkACF = either mkError identity . mkACF where
  mkError arrows = panic $ "unsafeMkACF: The following arrows are invalid: " <> show arrows

-- | Make a workflow state from a `Name`.
singletonWfState :: Name -> WorkflowState
singletonWfState = unsafeWorkflowState . S.singleton . makePlace

-- | Generate a unique name.
genName :: (MonadGen e m, Show e) => m Name
genName = Name . show <$> gen

-- | Generate a uniq workflow state.
genWfState :: (MonadGen e m, Show e) => m WorkflowState
genWfState = singletonWfState <$> genName

-- | Construct the list of transitions from a given `SafeWorkflow a`.
constructTransitions :: SafeWorkflow a -> [Transition]
constructTransitions = runGen . execWriterT . constructTransitionsM startState endState

-- | Construct the list of transitions from a given `SafeWorkflow a` `swf` and `start` and `end` states.
-- The open-ended transitions of `swf` will be connected to the `start` and `end` states
-- based on the stucture of `swf`.
constructTransitionsM :: WorkflowState -> WorkflowState -> SafeWorkflow a -> (WriterT [Transition] (Gen Integer)) ()
constructTransitionsM start end (AND _ _ branches) = do
  inOuts <- forM (GHC.toList branches) $ \br -> do
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
constructTransitionsM start end (ACF (M.toList -> acfList)) = do
  let acfArrows = map fst acfList
      acfPlaces = S.fromList
                . filter isIntermediate
                . concatMap (\(ACFArrow x y) -> [x,y])
                $ acfArrows
  stateMap <- sequence $ M.fromSet (const genWfState) acfPlaces
  let getState :: ACFPlace -> WorkflowState
      getState Entry = start
      getState Exit  = end
      getState p     = fromMaybe (panic $ "constructTransitionsM: Place " <> show p <> " is not present in state map.") $ M.lookup p stateMap
  forM_ acfList $ \(ACFArrow from to, swfs) -> do
    let from' = getState from
        to'   = getState to
    mapM_ (constructTransitionsM from' to') swfs
constructTransitionsM start end Atom{..} = do
  tell [Arrow start end]

instance Pretty ACFPlace where
  ppr Entry = "Entry"
  ppr Exit  = "Exit"
  ppr (P n) = "P" <+> ppr n

instance Arbitrary ACFPlace where
  arbitrary = oneof
    [ pure Entry
    , P <$> arbitrary
    , pure Exit
    ]

  shrink _ = []

instance Arbitrary ACFArrow where
  arbitrary = do
    from <- arbitrary @ACFPlace
    to   <- arbitrary @ACFPlace `suchThat` (\to -> from < to)
    pure $ ACFArrow from to

  shrink (ACFArrow from@(P _) to@(P _)) =
    [ ACFArrow Entry Exit
    , ACFArrow from  Exit
    , ACFArrow to    Exit
    , ACFArrow Entry to
    , ACFArrow Entry from
    ]
  shrink _ = []

instance Arbitrary a => Arbitrary (SafeWorkflow a) where
  arbitrary = sized genSWFNet where
    -- | Sized `SafeWorkflow generation
    genSWFNet :: Int -> QC.Gen (SafeWorkflow a)
    genSWFNet n |          n <= 1 = atom
    genSWFNet n | 1 < n && n <= 2 = maxComplexity 2
    genSWFNet n | 2 < n && n <= 3 = maxComplexity 3
    genSWFNet n | 3 < n && n <= 4 = maxComplexity 4
    genSWFNet n | 4 < n && n <= 5 = maxComplexity 5
    genSWFNet n | 5 < n           = unsafeMkACF <$> sizedACFMap n
    genSWFNet n = panic $ "Negative value for SafeWorkflow generation: " <> show n

    maxComplexity :: Int -> QC.Gen (SafeWorkflow a)
    maxComplexity n = oneof $ take n (allComplexities n)

    allComplexities :: Int -> [QC.Gen (SafeWorkflow a)]
    allComplexities n = [ atom, complexity2 n, complexity3 n, complexity4 n, complexity5 n, complexity6 n ]

    atom :: QC.Gen (SafeWorkflow a)
    atom = Atom <$> arbitrary

    complexity2 :: Int -> QC.Gen (SafeWorkflow a)
    complexity2 n = partitionThen n 2 $ \partition -> do
      case partition of
        [k1,k2] -> oneof
          [ XOR <$> genSWFNet k1 <*> genSWFNet k2
          , Seq <$> genSWFNet k1 <*> genSWFNet k2
          , SimpleLoop <$> genSWFNet k1 <*> genSWFNet k2
          ]
        _ -> panic $ show n <> " was not partitioned into 2 components"

    complexity3 :: Int -> QC.Gen (SafeWorkflow a)
    complexity3 n = partitionThen n 3 $ \partition -> do
      case partition of
        [k1,k2,k3] -> oneof
          [ Loop <$> genSWFNet k1 <*> genSWFNet k2 <*> genSWFNet k3
          ]
        _ -> panic $ show n <> " was not partitioned into 3 components"

    complexity4 :: Int -> QC.Gen (SafeWorkflow a)
    complexity4 n = partitionThen n 4 $ \partition -> do
      case partition of
        [k1,k2,k3,k4] -> oneof
          [ AND <$> arbitrary <*> arbitrary <*> someSWFNets (n-2) 2
          ]
        _ -> panic $ show n <> " was not partitioned into 4 components"

    complexity5 :: Int -> QC.Gen (SafeWorkflow a)
    complexity5 n = partitionThen n 5 $ \partition -> do
      case partition of
        [k1,k2,k3,k4,k5] -> oneof
          [ AND <$> arbitrary <*> arbitrary <*> someSWFNets (n-2) 3
          ]
        _ -> panic $ show n <> " was not partitioned into 5 components"

    complexity6 :: Int -> QC.Gen (SafeWorkflow a)
    complexity6 n = unsafeMkACF <$> sizedACFMap n

    -- | Generates `SafeWorkflow`s of summed size `n`.
    someSWFNets :: Int -> Int -> QC.Gen (List2 (SafeWorkflow a))
    someSWFNets n k =
      partitionThen n k $ \ps -> do
        xs <- mapM genSWFNet ps
        pure $ GHC.fromList xs

  -- FIXME: Annotating the Atom with the split annotation of the AND is very ad hoc.
  shrink x@AND{..} = Atom splitAnnot : genericShrink x
  shrink x = genericShrink x

-- | `paritionThen n k gen` picks a random partition of `n` containing exactly `k` number of elements where `1 < k <= n`,
-- then runs a generators with the partition. Used for sized generation of safe workflows.
-- The initial size is partitioned into some other sizes used for generating the subworkflows.
partitionThen :: Int -> Int -> ([Int] -> QC.Gen a) -> QC.Gen a
partitionThen n k g = do
  let partitions = filter ((==k) . length) $ partitionInt n
  case partitions of
    [] -> panic $ "partitionThen: Couldn't partition " <> show n <> " into " <> show k <> " integers"
    _  -> do
      aPartition <- elements partitions
      g aPartition

-- | `partitionInt n k` partitions the integer `n` into `k` number of integers
-- in all possible ways where `1 < k <= n`.
partitionInt :: Int -> [[Int]]
partitionInt d = go d d where
  go _  0  = [[]]
  go !h !n = [ a:as | a<-[1..min n h], as <- go a (n-a) ]

-- | Sized generation of acyclic control-flow map.
sizedACFMap :: forall a . Arbitrary a => Int -> QC.Gen (ACFMap a)
sizedACFMap size = do
  -- NOTE: number of new places in the spine
  spineLength <- frequency
    [ (30, pure 0)
    , (25, pure 1)
    , (20, pure 2)
    , (10, pure 3)
    , (5,  pure 4)
    , (3,  pure 5)
    , (3,  pure 6)
    , (2,  pure 7)
    , (2,  pure 8)
    ]
  let spineLength' = min (size-1) spineLength -- NOTE: length of spine can't be larger than the size (size ~ no. transitions)
      spineLabels  = map (*10^10) [1..spineLength']
      spinePlaces  = [Entry] ++ map P spineLabels ++ [Exit]
      spineArrows  = zipWith ACFArrow spinePlaces (drop 1 spinePlaces)

  -- NOTE: there will be at least `spineLength' + 1` workflows in the spine
  --       and there can be at most `size` workflows together with the extensions
  numPartitions <- choose (spineLength' + 1, size)

  partitionThen size numPartitions $ \partition -> do
    let spineSizes     = take (spineLength' + 1) partition
        extensionSizes = drop (spineLength' + 1) partition
    spineSwfs <- forM spineSizes $ \s -> do
      swf <- resize s (arbitrary @(SafeWorkflow a))
      pure [swf]
    let acfMap = M.fromList $ zip spineArrows spineSwfs
    foldM (flip extendACFMap) acfMap extensionSizes

-- | Extends an ACF with either a direct arrow or a new place.
-- This will only generate ACFs with single branched transitions.
-- However, those single branches can contain other workflows suchs as loops, ANDs and even other ACFs.
-- Some special cases of ACF generation are hard-coded (eg.: XOR, Seq) which guarantees that we will have
-- XOR-splits, so we don't lose generality.
extendACFMap :: Arbitrary a => Int -> ACFMap a -> QC.Gen (ACFMap a)
extendACFMap size acfMap
  | size <= 1 = extendACFMapWithArrow size acfMap
  | otherwise = oneof [ extendACFMapWithArrow size acfMap
                      , extendACFMapWithPlace size acfMap
                      ]

-- | Extends a ACF with a direct arrow.
extendACFMapWithArrow :: forall a . Arbitrary a => Int -> ACFMap a -> QC.Gen (ACFMap a)
extendACFMapWithArrow size acfMap = do
  let places = S.toList $ gACFPlaces acfMap
  x   <- elements places `suchThat` (\x -> x /= Exit)
  y   <- elements places `suchThat` (\y -> x < y)
  swf <- resize size (arbitrary @(SafeWorkflow a))
  pure $ M.insert (ACFArrow x y) [swf] acfMap

-- | Extends an ACF with a new place and two arrows connecting the place to the other parts of the ACF.
extendACFMapWithPlace :: forall a . Arbitrary a => Int -> ACFMap a -> QC.Gen (ACFMap a)
extendACFMapWithPlace size acfMap = do
  let places = S.toList $ gACFPlaces acfMap
  from <- elements places `suchThat` (\from -> from /= Exit)
  to   <- elements places `suchThat` (\to   -> to   /= Entry && canFitBetween from to && from < to)
  p    <- between 0 maxBound from to -- NOTE: this can crash if to == Exit and many values have been generated around maxBounds

  k1 <- choose (1,size-1)
  let k2 = size - k1

  swf1 <- resize k1 (arbitrary @(SafeWorkflow a))
  swf2 <- resize k2 (arbitrary @(SafeWorkflow a))

  pure $ acfMap <> M.fromList
    [ (ACFArrow from p, [swf1])
    , (ACFArrow p   to, [swf2])
    ]

-- | Checks whether two given places can accomodate and additional place in-between.
canFitBetween :: ACFPlace -> ACFPlace -> Bool
canFitBetween from to
  | from == to = False
canFitBetween Entry _ = True
canFitBetween _ Exit  = True
canFitBetween (P from) (P to) = not $ hasIncorrectBounds from to
canFitBetween _ _ = False

-- | Generates a new ACF place between two other places with
-- respect to some inclusive bounds. The additional bounds are just
-- for the sake generality.
between :: Int -> Int -> ACFPlace -> ACFPlace -> QC.Gen ACFPlace
between lo hi _ _
  | hasIncorrectBounds lo hi = panic $ boundError lo hi
between _ _ Entry Entry = panic $ boundError Entry Entry
between _ _ Exit Exit   = panic $ boundError Exit  Exit
between lo hi Entry Exit =
  P <$> choose (lo, hi)
between lo _ Entry (P high)
  | hasIncorrectBounds lo high = panic $ boundError lo high
  | otherwise = P <$> choose (lo, high-1)
between _ _ (P low) (P high)
  | hasIncorrectBounds low high = panic $ boundError low high
  | otherwise = P <$> choose (low+1, high-1)
between _ hi (P low) Exit
  | hasIncorrectBounds low hi = panic $ boundError low hi
  | otherwise = P <$> choose (low+1, hi)
between _ _ l h = panic $ boundError l h

boundError :: (Pretty a, Pretty b) => a -> b -> Text
boundError lo hi = show $ "between: Can't generate new place in between" <+> ppr lo <+> "and" <+> ppr hi

-- | Given two integers `n` and `k` checks whether `|n - k| < 2`.
-- Used for determining whetehr two ACF places can accomodate an additional place in-between.
hasIncorrectBounds :: Int -> Int -> Bool
hasIncorrectBounds lo hi = abs (hi - lo) < 2
