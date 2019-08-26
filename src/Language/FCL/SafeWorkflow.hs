{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
module Language.FCL.SafeWorkflow
  ( SafeWorkflow(Atom, AND, GenLoop)
  , AnnTransition(..)
  , AnnTransitions(..)

  -- | Acyclic control-flow types.
  , PlaceId
  , ACFArrow(..)
  , ACFPlace(..)
  , ACFMap
  , PlaceAnnotMap

  -- | AND-split.
  , ANDBranch(..)

  -- | Pattern synonyms.
  -- `AND2` and `GenXOR` patterns are only available for `SimpleWorkflow`s.
  -- See `Language.FCL.SafeWorkflow.Simple`.
  , pattern XOR
  , pattern XOR3
  , pattern Seq
  , pattern SimpleLoop
  , pattern Loop
  , pattern ACF

  -- | Selector functions
  , andBranches
  , gLoopIn
  , gLoopOut
  , gLoopExit
  , xorLhs
  , xorRhs
  , seqLhs
  , seqRhs

  -- | Acyclic control-flow types smart constructors,
  , mkACF
  , unsafeMkACF

  -- | Transforming a `SafeWorkflow` into a set of transitions.
  , constructTransitions
  , constructAnnTransitions

  -- | Other helper functions.
  , collectPlaceIds
  , noMatchError
  , mapANDBranchWF
  ) where

import Protolude

import qualified GHC.Exts as GHC (IsList(..))

import Data.Set (Set, (\\))
import Data.Map (Map)
import Data.List.List2 (List2(..))
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.Gen
import Control.Monad.Writer

import Test.QuickCheck hiding (Gen)
import qualified Test.QuickCheck as QC

import Language.FCL.AST hiding ((\\))
import Language.FCL.Pretty (Pretty, ppr, (<+>))
import Language.FCL.Orphans()


-- NOTE: can't return back to different places from a loop (eg.: novation.s)
-- NOTE: syncronisation points are always singleton states (places) (eg.: novation.s)
-- NOTE: safe workflows are always progressive, you can't jump into a loop (loops are isolated)
--       GenXOR allowed this, but ACF does not

-- | Identifiers for places in `ACFMap`s
type PlaceId = Int

-- | An ordered place in a general acyclic control-flow.
data ACFPlace = Entry     -- ^ Entry point to the acyclic control-flow. Precedes every other place.
              | P PlaceId -- ^ An intermediate place whose ordering is defined by its identifier.
              | Exit      -- ^ Exit point from the acyclic control-flow. Succeeds every other place.
  deriving (Eq, Ord, Show, Generic, NFData)

-- | An arrow between two places in an acyclic control-flow.
-- The first component is always strictly less than the second component.
data ACFArrow = ACFArrow ACFPlace ACFPlace
  deriving (Eq, Ord, Show, Generic, NFData)

-- | A mapping describing the structure of an acyclic control-flow.
-- It maps arrows to a non-empty list of safe workflows. An arrow represents
-- the possible ways to transition from a given state to another one. If the list
-- is not singleton, it represents an XOR-split, multiple ways to exectute the transition.
type ACFMap a b = Map ACFArrow (NonEmpty (SafeWorkflow a b))

-- | A mapping describing the annotations of places in `ACFMap`s.
type PlaceAnnotMap a = Map PlaceId a

-- | Branch of an AND-split.
data ANDBranch a b = ANDBranch
  { branchInAnnot  :: a                 -- ^ Annotation going into the branch.
  , branchOutAnnot :: a                 -- ^ Annotation going out of the branch.
  , branchWorkflow :: SafeWorkflow a b  -- ^ Workflow in the branch.
  } deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)

-- NOTE: initial and terminal places cannot be annotated
-- | Workflow nets that are sound by construction. We only allow these _safe_ workflow nets
-- to be constructed in very specific ways in order to make soundness verification automatic.
-- The transitions in safe workflows can be annotated by any desired information.
data SafeWorkflow a b
  -- | AND splitting into multiple workflows.
  = AND { splitAnnot  :: b                            -- ^ Annotation for the AND-split transition
        , joinAnnot   :: b                            -- ^ Annotation for the AND-join transition
        , andBranches :: List2 (ANDBranch a b)        -- ^ At least one branch
        }
  -- | Looping construct with option to exit from both the body of the loop and the head of the loop.
  -- If the first half of the body is empty, we exit from head, otherwise exit from the body.
  | GenLoop { exitAnnot :: Maybe a                      -- ^ Annotation of the exit point (place)
            , gLoopIn   :: Maybe (SafeWorkflow a b)     -- ^ First half of the body of the loop
            , gLoopExit :: (SafeWorkflow a b)           -- ^ Exit from the loop
            , gLoopOut  :: (SafeWorkflow a b)           -- ^ Seconds half of the body of the loop
            }
  -- | Acyclic control-flow.
  -- It maps arrows to a non-empty list of safe workflows. An arrow represents
  -- the possible ways to transition from a given state to another one. If the list
  -- is not singleton, it represents an XOR-split, multiple ways to exectute the transition.
  | ACF' { placeAnnots :: PlaceAnnotMap a     -- ^ Anotations for the places in the acyclic control-flow.
         , acfMap      :: ACFMap a b          -- ^ A mapping describing the structure of the acyclic control-flow.
         }
  -- | Atom representing a single transition.
  | Atom { atomAnnot :: b                             -- ^ Annotation for the transitions
         }
  deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)


-- TODO: redefine these using record pattern synonyms (once they are available)
-- https://gitlab.haskell.org/ghc/ghc/wikis/pattern-synonyms/record-pattern-synonyms

{-# COMPLETE AND, GenLoop,          ACF, Atom #-}
{-# COMPLETE AND, SimpleLoop, Loop, ACF, Atom #-}

-----------------------------------------------------------------
-- Pattern synonyms, selector functions and smart constructors --
-----------------------------------------------------------------

-- | XOR with two branches.
pattern XOR :: SafeWorkflow a b -> SafeWorkflow a b -> SafeWorkflow a b
pattern XOR lhs rhs <- ACF' (M.toList -> []) (M.toList -> [(ACFArrow Entry Exit, [lhs, rhs])])
  where XOR lhs rhs  = ACF' mempty (M.fromList  [(ACFArrow Entry Exit, [lhs, rhs])])

-- | XOR with three branches.
pattern XOR3 :: SafeWorkflow a b -> SafeWorkflow a b -> SafeWorkflow a b -> SafeWorkflow a b
pattern XOR3 a b c   = XOR a (XOR b c)

-- | Sequencing two safe workflows.
pattern Seq :: a -> SafeWorkflow a b -> SafeWorkflow a b -> SafeWorkflow a b
pattern Seq ann lhs rhs <- ACF' (M.toList -> [(1,ann)]) (M.toList -> [(ACFArrow Entry (P 1), [lhs]), (ACFArrow (P 1) Exit, [rhs])])
  where Seq ann lhs rhs =  ACF' (M.fromList  [(1,ann)]) (M.fromList  [(ACFArrow Entry (P 1), [lhs]), (ACFArrow (P 1) Exit, [rhs])])

-- | Unidirectional pattern general acyclic control-flow.
pattern ACF :: PlaceAnnotMap a -> ACFMap a b -> SafeWorkflow a b
pattern ACF annots acfMap <- ACF' annots acfMap

-- | Simple loop with exit only from the head.
pattern SimpleLoop :: SafeWorkflow a b -> SafeWorkflow a b -> SafeWorkflow a b
pattern SimpleLoop loop exit = GenLoop Nothing Nothing exit loop

-- | Loop with exit from the body.
pattern Loop :: a -> SafeWorkflow a b -> SafeWorkflow a b -> SafeWorkflow a b -> SafeWorkflow a b
pattern Loop exitAnnot loopIn exit loopOut = GenLoop (Just exitAnnot) (Just loopIn) exit loopOut

noMatchError :: Text -> Text
noMatchError selector = "No match in record selector " <> selector

xorLhs :: SafeWorkflow a b -> SafeWorkflow a b
xorLhs (XOR lhs _) = lhs
xorLhs _ = panic $ noMatchError "xorLhs"

xorRhs :: SafeWorkflow a b -> SafeWorkflow a b
xorRhs (XOR _ rhs) = rhs
xorRhs _ = panic $ noMatchError "xorRhs"

seqLhs :: SafeWorkflow a b -> SafeWorkflow a b
seqLhs (Seq _ lhs _) = lhs
seqLhs _ = panic $ noMatchError "seqLhs"

seqRhs :: SafeWorkflow a b -> SafeWorkflow a b
seqRhs (Seq _ _ rhs) = rhs
seqRhs _ = panic $ noMatchError "seqRhs"

seqAnnot :: SafeWorkflow a b -> a
seqAnnot (Seq ann _ _) = ann
seqAnnot _ = panic $ noMatchError "seqAnnot"

-- | Is an ACF place intermediate (not `Entry` nor `Exit`).
isIntermediate :: ACFPlace -> Bool
isIntermediate (P _) = True
isIntermediate _     = False

-- | Gather the places from an ACF map.
gACFPlaces :: ACFMap a b -> Set ACFPlace
gACFPlaces = S.fromList . concatMap (\(ACFArrow x y) -> [x,y]) . M.keys

-- | Collects all the invalid arrows in a general acyclic control-flow (non-recursive).
-- An arrow is invalid if it loops back to the same place,
-- or goes back to a place preceeding the source place.
collectInvalidArrows :: ACFMap a b -> Set ACFArrow
collectInvalidArrows acfMap = S.fromList $ filter isInvalid acfArrows where
  isInvalid :: ACFArrow -> Bool
  isInvalid (ACFArrow from to) = from >= to

  acfArrows :: [ACFArrow]
  acfArrows = map fst . M.toList $ acfMap

-- TODO: Pretty instance
data ACFErrors = ACFErrors
  { additionalPlaces :: Set PlaceId   -- ^ Places present in annotations map but missing from ACF map.
  , uncoveredPlaces  :: Set PlaceId   -- ^ Places missing from annotations map but present in ACF map.
  , invalidArrows    :: Set ACFArrow  -- ^ An arrow looping back to the same place, or going back to a previous place.
  } deriving (Eq, Ord, Show)

collectACFErrors :: PlaceAnnotMap a -> ACFMap a b -> ACFErrors
collectACFErrors annots acfMap =
  ACFErrors (annotIds \\ placeIds)
            (placeIds \\ annotIds)
            (collectInvalidArrows acfMap)

  where
    annotIds :: Set PlaceId
    annotIds = M.keysSet annots

    placeIds :: Set PlaceId
    placeIds = collectPlaceIds acfMap

-- | Collect the `PlaceId`s from a given `ACFMap`.
-- The entry and exit points are nt checked, since they
-- already should have been labelled by the outer construct.
collectPlaceIds :: ACFMap a b -> Set PlaceId
collectPlaceIds = S.fromList . concatMap getIds . M.keys where

  getIds :: ACFArrow -> [PlaceId]
  getIds (ACFArrow (P id1) (P id2)) = [id1, id2]
  getIds (ACFArrow _       (P id2)) = [id2]
  getIds (ACFArrow (P id1)       _) = [id1]
  getIds _ = []


-- | Safe smart constructor for ACFs. Returns a safe workflow
-- if the ACF is correct, or the list of invalid arrows if it is not.
mkACF :: PlaceAnnotMap a -> ACFMap a b -> Either ACFErrors (SafeWorkflow a b)
mkACF annots acfMap = case collectACFErrors annots acfMap of
  ACFErrors{..} | null additionalPlaces || null uncoveredPlaces || null invalidArrows
    -> Right $ ACF' annots acfMap
  errs -> Left errs

-- | Unsafe smart constructor for ACFs. Returns a safe workflow
-- if the ACF is correct, crashes if it is not.
unsafeMkACF :: PlaceAnnotMap a -> ACFMap a b -> SafeWorkflow a b
unsafeMkACF annots = either mkError identity . mkACF annots where
  mkError acfErrors = panic $ "unsafeMkACF: There were some erros during the construction of the ACF: " <> show acfErrors

---------------------------------------------------
-- Constructing transitions from @SafeWorkflow@s --
---------------------------------------------------

-- | Make a workflow state from a `Name`.
singletonWfState :: Place -> WorkflowState
singletonWfState = unsafeWorkflowState . S.singleton

-- | Generate a unique name.
genName :: (MonadGen e m, Show e) => m Name
genName = Name . show <$> gen

genPlace :: (MonadGen e m, Show e) => m Place
genPlace = Place <$> genName

-- | Generate a unique workflow state.
genWfState :: (MonadGen e m, Show e) => m WorkflowState
genWfState = singletonWfState <$> genPlace

data AnnTransition a = AnnTransition
  { getAnnot :: a
  , getTrans :: Transition
  } deriving (Eq, Ord, Show)

data AnnTransitions a b = AnnTransitions
  { getPlaceAnnotations :: Map Place a
  , getTransitions      :: [AnnTransition b]
  } deriving (Eq, Ord, Show)

instance Semigroup (AnnTransitions a b) where
  (<>) (AnnTransitions as1 ts1) (AnnTransitions as2 ts2) =
    AnnTransitions (as1 <> as2) (ts1 <> ts2)

instance Monoid (AnnTransitions a b) where
  mempty = AnnTransitions mempty mempty

type TransitionCtorM a b = WriterT (AnnTransitions a b) (Gen Integer)

execTransitionConstructor :: TransitionCtorM a b c -> AnnTransitions a b
execTransitionConstructor = runGen . execWriterT

addPlaceAnnotM :: Place -> a -> TransitionCtorM a b ()
addPlaceAnnotM place annot = do
  let newEntry = AnnTransitions (M.singleton place annot) mempty
  tell newEntry

addTransitionM :: AnnTransition b -> TransitionCtorM a b ()
addTransitionM t = do
  let newEntry = AnnTransitions mempty [t]
  tell newEntry

initAnnotationsM :: a -> a -> TransitionCtorM a b ()
initAnnotationsM initAnn termAnn = do
  addPlaceAnnotM PlaceStart initAnn
  addPlaceAnnotM PlaceEnd   termAnn

-- NOTE: Specialized to disambiguate the overlaoded list type variable.
-- | Add multiple transitions.
addTransitionsM :: [AnnTransition b] -> TransitionCtorM a b ()
addTransitionsM = mapM_ addTransitionM

-- | Construct a list of transitions without annotations from a given `SafeWorkflow`.
constructTransitions :: a -> a -> SafeWorkflow a b -> [Transition]
constructTransitions initAnn termAnn
  = map getTrans
  . getTransitions
  . constructAnnTransitions initAnn termAnn

-- | Construct a list of annotated transitions from a given `SafeWorkflow`.
constructAnnTransitions :: a -> a -> SafeWorkflow a b -> AnnTransitions a b
constructAnnTransitions initAnn termAnn swf = execTransitionConstructor $ do
  initAnnotationsM initAnn termAnn
  constructAnnTransitionsM startState endState swf

-- TODO: implement place-annotated construction
-- | Construct the list of transitions from a given `SafeWorkflow` @swf@ and @start@ and @end@ states.
-- The open-ended transitions of @swf@ will be connected to the @start@ and @end@ states
-- based on the stucture of @swf@.
constructAnnTransitionsM :: WorkflowState -> WorkflowState -> SafeWorkflow a b -> TransitionCtorM a b ()
constructAnnTransitionsM start end (AND splitAnn joinAnn branches) = do
  inOuts <- forM (GHC.toList branches) $ \(ANDBranch inAnn outAnn swf) -> do
    inPlace  <- genPlace
    outPlace <- genPlace
    addPlaceAnnotM inPlace  inAnn
    addPlaceAnnotM outPlace outAnn
    let inState  = singletonWfState inPlace
        outState = singletonWfState outPlace
    constructAnnTransitionsM inState outState swf
    pure (inState, outState)
  let (ins, outs) = unzip inOuts
  addTransitionsM
    [ AnnTransition splitAnn $ Arrow start (mconcat ins)
    , AnnTransition joinAnn  $ Arrow (mconcat outs) end
    ]
constructAnnTransitionsM start end (SimpleLoop loop exit) = do
  constructAnnTransitionsM start start loop
  constructAnnTransitionsM start end   exit
constructAnnTransitionsM start end (Loop exitAnn gLoopIn exit gLoopOut) = do
  exitPlace <- genPlace
  addPlaceAnnotM exitPlace exitAnn
  let exitState = singletonWfState exitPlace
  constructAnnTransitionsM start exitState gLoopIn
  constructAnnTransitionsM exitState end   exit
  constructAnnTransitionsM exitState start gLoopOut
constructAnnTransitionsM start end (ACF annotMap (M.toList -> acfList)) = do
  let acfArrows = map fst acfList
      acfPlaces = S.fromList
                . filter isIntermediate
                . concatMap (\(ACFArrow x y) -> [x,y])
                $ acfArrows
  stateMap <- sequence $ flip M.fromSet acfPlaces $ \case
    Entry -> genWfState -- TODO: is this correct?
    Exit -> genWfState
    P id -> do
      p <- genPlace
      let errMsg = "constructAnnTransitionsM: Couldn't find Place with id " <> show id <> " in annotation map of ACF."
          annot = fromMaybe (panic errMsg) (M.lookup id annotMap)
      addPlaceAnnotM p annot
      pure $ singletonWfState p
  let getState :: ACFPlace -> WorkflowState
      getState Entry = start
      getState Exit  = end
      getState p     = flip fromMaybe (M.lookup p stateMap) $
        panic $ "constructAnnTransitionsM: Place " <> show p <> " is not present in state map."
  forM_ acfList $ \(ACFArrow from to, swfs) -> do
    let from' = getState from
        to'   = getState to
    mapM_ (constructAnnTransitionsM from' to') swfs
constructAnnTransitionsM start end Atom{..} =
  addTransitionM $ AnnTransition atomAnnot $ Arrow start end

-------------------------
-- Arbitrary instances --
-------------------------

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (ANDBranch a b) where
  arbitrary = do
    wfSize <- getSize
    swf    <- resize wfSize $ arbitrary @(SafeWorkflow a b)
    annIn  <- arbitrary @a
    annOut <- arbitrary @a
    pure $ ANDBranch annIn annOut swf

  shrink = genericShrink

instance (Arbitrary a, Arbitrary b) => Arbitrary (SafeWorkflow a b) where
  arbitrary = sized genSWFNet where
    -- | Sized `SafeWorkflow generation
    genSWFNet :: Int -> QC.Gen (SafeWorkflow a b)
    genSWFNet n |          n <= 1 = atom
    genSWFNet n | 1 < n && n <= 2 = maxComplexity 2
    genSWFNet n | 2 < n && n <= 3 = maxComplexity 3
    genSWFNet n | 3 < n && n <= 4 = maxComplexity 4
    genSWFNet n | 4 < n && n <= 5 = maxComplexity 5
    genSWFNet n | 5 < n           = complexity6 n
    genSWFNet n = panic $ "Negative value for SafeWorkflow generation: " <> show n

    maxComplexity :: Int -> QC.Gen (SafeWorkflow a b)
    maxComplexity n = oneof $ take n (allComplexities n)

    allComplexities :: Int -> [QC.Gen (SafeWorkflow a b)]
    allComplexities n = [ atom, complexity2 n, complexity3 n, complexity4 n, complexity5 n, complexity6 n ]

    atom :: QC.Gen (SafeWorkflow a b)
    atom = Atom <$> arbitrary

    complexity2 :: Int -> QC.Gen (SafeWorkflow a b)
    complexity2 n = partitionThen n 2 $ \partition -> do
      case partition of
        [k1,k2] -> oneof
          [ XOR <$> genSWFNet k1 <*> genSWFNet k2
          , Seq <$> arbitrary <*> genSWFNet k1 <*> genSWFNet k2
          , SimpleLoop <$> genSWFNet k1 <*> genSWFNet k2
          ]
        _ -> panic $ show n <> " was not partitioned into 2 components"

    complexity3 :: Int -> QC.Gen (SafeWorkflow a b)
    complexity3 n = partitionThen n 3 $ \partition -> do
      case partition of
        [k1,k2,k3] -> oneof
          [ Loop <$> arbitrary <*> genSWFNet k1 <*> genSWFNet k2 <*> genSWFNet k3
          ]
        _ -> panic $ show n <> " was not partitioned into 3 components"

    complexity4 :: Int -> QC.Gen (SafeWorkflow a b)
    complexity4 n = partitionThen n 4 $ \partition -> do
      case partition of
        [k1,k2,k3,k4] -> oneof
          [ AND <$> arbitrary <*> arbitrary <*> sizedANDBranches (n-2) 2
          ]
        _ -> panic $ show n <> " was not partitioned into 4 components"

    complexity5 :: Int -> QC.Gen (SafeWorkflow a b)
    complexity5 n = partitionThen n 5 $ \partition -> do
      case partition of
        [k1,k2,k3,k4,k5] -> oneof
          [ AND <$> arbitrary <*> arbitrary <*> sizedANDBranches (n-2) 3
          ]
        _ -> panic $ show n <> " was not partitioned into 5 components"

    complexity6 :: Int -> QC.Gen (SafeWorkflow a b)
    complexity6 n = do
      acfMap <- sizedACFMap n
      let placeIds = collectPlaceIds acfMap
      annots <- sequence $ M.fromSet (\_ -> arbitrary) placeIds
      pure $ unsafeMkACF annots acfMap

    -- | Generates `SafeWorkflow`s of summed size `n`.
    sizedANDBranches
      :: forall a b . (Arbitrary a, Arbitrary b)
      => Int  -- ^ Maximum number of transitions in the entire AND-split (all branches together)
      -> Int  -- ^ Number of branches
      -> QC.Gen (List2 (ANDBranch a b))
    sizedANDBranches n k =
      partitionThen n k $ \ps -> do
        branches <- forM ps $ \s -> resize s (arbitrary @(ANDBranch a b))
        pure $ GHC.fromList branches


  -- FIXME: Annotating the Atom with the split annotation of the AND is very ad hoc.
  --        This should be only a structural shrink unrelated to the annotation.
  --        We know how it simplifies the structure but don't know how it should simplify the annotation.
  -- Possible Fix: Monoid constraint for the annotations?
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
sizedACFMap :: forall a b. (Arbitrary a, Arbitrary b) => Int -> QC.Gen (ACFMap a b)
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
      swf <- resize s (arbitrary @(SafeWorkflow a b))
      pure [swf]
    let acfMap = M.fromList $ zip spineArrows spineSwfs
    foldM (flip extendACFMap) acfMap extensionSizes

-- | Extends an ACF with either a direct arrow or a new place.
-- This will only generate ACFs with single branched transitions.
-- However, those single branches can contain other workflows suchs as loops, ANDs and even other ACFs.
-- Some special cases of ACF generation are hard-coded (eg.: XOR, Seq) which guarantees that we will have
-- XOR-splits, so we don't lose generality.
extendACFMap :: (Arbitrary a, Arbitrary b) => Int -> ACFMap a b -> QC.Gen (ACFMap a b)
extendACFMap size acfMap
  | size <= 1 = extendACFMapWithArrow size acfMap
  | otherwise = oneof [ extendACFMapWithArrow size acfMap
                      , extendACFMapWithPlace size acfMap
                      ]

-- | Extends a ACF with a direct arrow.
extendACFMapWithArrow :: forall a b. (Arbitrary a, Arbitrary b) => Int -> ACFMap a b -> QC.Gen (ACFMap a b)
extendACFMapWithArrow size acfMap = do
  let places = S.toList $ gACFPlaces acfMap
  x   <- elements places `suchThat` (\x -> x /= Exit)
  y   <- elements places `suchThat` (\y -> x < y)
  swf <- resize size (arbitrary @(SafeWorkflow a b))
  pure $ M.insert (ACFArrow x y) [swf] acfMap

-- | Extends an ACF with a new place and two arrows connecting the place to the other parts of the ACF.
extendACFMapWithPlace :: forall a b. (Arbitrary a, Arbitrary b) => Int -> ACFMap a b -> QC.Gen (ACFMap a b)
extendACFMapWithPlace size acfMap = do
  let places = S.toList $ gACFPlaces acfMap
  from <- elements places `suchThat` (\from -> from /= Exit)
  to   <- elements places `suchThat` (\to   -> to   /= Entry && canFitBetween from to && from < to)
  p    <- between 0 maxBound from to -- NOTE: this can crash if to == Exit and many values have been generated around maxBounds

  k1 <- choose (1,size-1)
  let k2 = size - k1

  swf1 <- resize k1 (arbitrary @(SafeWorkflow a b))
  swf2 <- resize k2 (arbitrary @(SafeWorkflow a b))

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

---------------------
-- Other instances --
---------------------

mapANDBranchWF :: (SafeWorkflow a b -> SafeWorkflow a b) -> ANDBranch a b -> ANDBranch a b
mapANDBranchWF f andBranch@ANDBranch{..} = andBranch { branchWorkflow = f branchWorkflow }

instance Bifunctor ANDBranch where
  bimap f g (ANDBranch inAnnot outAnnot swf) =
    ANDBranch (f inAnnot) (f outAnnot) (bimap f g swf)

-- QUESTION: Isn't there a clever way to derive these?
-- ANSWER: Data.Bifunctor.TH doesn't work due to not so clever deriving machinery (need Bifunctor instance for Map ...).
--         Swapping type variables for Safeworkflow trough newtypes or data then deriving the instances doesn't work either.
instance Bifunctor SafeWorkflow where
  first f andSplit@AND{..} = andSplit { andBranches = fmap (first f) andBranches }
  first f loop@GenLoop{..} = loop { exitAnnot = fmap f exitAnnot
                                  , gLoopIn   = fmap (first f) gLoopIn
                                  , gLoopExit = first f gLoopExit
                                  , gLoopOut  = first f gLoopOut
                                  }
  first f acf@ACF'{..} = acf { placeAnnots = M.map f placeAnnots
                             , acfMap = M.map (fmap $ first f) acfMap
                             }
  first f (Atom ann) = Atom ann

  second = fmap
