{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
module Language.FCL.SafeWorkflow.Editable
  ( Continuation(..)
  , CGMetadata(..)
  , TEditLabel(..)
  , PEditLabel(..)
  , PrettyTLabel(..)
  , EditableSW
  , PlaceId
  , TransId
  , ANDBranchLabels(..)

  , pattern Hole

  , replaceHole
  , prettify
  , noLoc
  )
  where

import Protolude

import qualified GHC.Exts as GHC (IsList(..))
import qualified GHC.Show (show)

import Data.Text (unlines)
import Data.List.List2

import qualified Data.Set as S
import qualified Data.Map as M

import Language.FCL.SafeWorkflow hiding
  ( Atom
  , AND
  , pattern SimpleLoop
  , pattern Loop
  , pattern XOR
  , pattern Seq
  , pattern ACF
  , PlaceId
  )
import Language.FCL.AST (Name(..), Transition(..), WorkflowState(..), Place(..), Expr(..), BinOp(..), Located(..), Loc(..))
import Language.FCL.Pretty (Doc, Pretty, ppr, hsep, prettyPrint)
import Language.FCL.Analysis (inferStaticWorkflowStates)
import Language.FCL.Graphviz hiding (AnnotatedTransition)
import Language.FCL.SafeWorkflow.Simple (constructTransitionsWithoutPlaces, constructAnnTransitionsWithoutPlaces)

import qualified Language.FCL.SafeWorkflow as SW
import qualified Language.FCL.Graphviz     as GV

-- TODO: Separate metadata (name) from the datastructure.
-- Only store IDs and maintain a metadata table.
-- TODO: Maybe only do this for methods (e.g.: preconditions)

-- NOTE: Reediting an already finished transition could be done by
-- transforming it back to a Hole first, then editing it.
-- NOTE: If we ever allow reediting, we will need a smarter way to index holes,
-- because the current implementation would generate clashing IDs


-- | Identifier of a transition in an editable workflow
type TransId = Int

-- NOTE: global would be for methods (e.g.: preconditions)
-- | Local transiion metadata forcode generation
data CGMetadata = CGMetadata
  { cgmCode   :: Maybe Expr   -- ^ Code to be generated into the transition (`Nothing` -> `ENoOp`)
  , cgmIfCond :: Maybe Expr   -- ^ Possible @If@ condition for deterministic branhcing (NOTE: the conditions in a given method should always be mutually exclusive and should always cover the entire event-space)
  }
  deriving (Eq, Ord, Show)

-- TODO: add IF condition annotations (ids to group if conditions)
-- TODO: separate continuations from edit labels better
-- | Transition labels for safe workflow editing.
data TEditLabel = TEL
  { trId          :: TransId      -- ^ Unique identifier for the transition
  , trMethodName  :: Name         -- ^ Name of the method this transition is part of (can be non-unique across transitions)
  , trIsEditable  :: Bool         -- ^ Only for rendering
  , trCGMetadata  :: CGMetadata   -- ^ Metadata for code generation
  }
  deriving (Eq, Ord, Show)

-- | Newtype wrapper for pretty pritning `TEditLabel`s
newtype PrettyTLabel = PrettyTLabel TEditLabel
  deriving (Eq, Ord)

instance Pretty PrettyTLabel where
  ppr = \case
    PrettyTLabel TEL{..}
      | trIsEditable -> "_" <> ppr trId
      | otherwise    -> ppr trMethodName

instance Show PrettyTLabel where
  show = show . ppr

-- | Identifier of a place in an editable workflow.
type PlaceId = Int

-- | Place labels for safe workflow editing.
data PEditLabel
  -- | Label for a place
  = LPlace { lPlaceName :: Name      -- ^ Name of the place (used for rendering and code generation)
           , lPlaceId   :: PlaceId   -- ^ Identfier of the place (only used internally)
           }
  -- TODO: we might need IDs for these too
  | LInitial              -- ^ Label for the initial place
  | LTerminal             -- ^ Label for the terminal place
  deriving (Eq, Ord, Show)

-- | Newtype wrapper for pretty pritning `PEditLabel`s
newtype PrettyPLabel = PrettyPLabel PEditLabel
  deriving (Eq, Ord)

instance Pretty PrettyPLabel where
  ppr = \case
    PrettyPLabel (LPlace name _) -> ppr name
    -- TODO: magic names
    PrettyPLabel LInitial        -> "__initial__"
    PrettyPLabel LTerminal       -> "__terminal__"

instance Show PrettyPLabel where
  show = show . ppr

-- | Safe workflow enriched with additional annotations
-- to faciliate the editing process.
type EditableSW = SafeWorkflow PEditLabel TEditLabel

-- | Safe workflow enriched with additional annotations
-- to faciliate the editing process. This is the same as
-- `EditableSW` just with prettier labels.
type PrettyEditableSW = SafeWorkflow PrettyPLabel PrettyTLabel

prettify :: EditableSW -> PrettyEditableSW
prettify = bimap PrettyPLabel PrettyTLabel

-- | A `Hole` is an `Atom` annotated with a hole label.
-- These are the plugin points of the workflows, this where
-- the workflow can be edited.
pattern Hole :: TransId -> EditableSW
pattern Hole id <- SW.Atom (TEL id _ True _)
  where Hole id = SW.Atom $ TEL id (Name $ "_" <> show id) True (CGMetadata Nothing Nothing)

-- -- | A `Hole` with a `Maybe` condition.
-- pattern MCHole :: TransId -> Expr -> EditableSW
-- pattern MCHole id mCond <- SW.Atom (TEL id _ True _(CGMetadata Nothing mCond)
--   where MCHole id mCond =  SW.Atom $ TEL id (Name $ "_" <> show id) True (CGMetadata Nothing mCond)

-- | Labels for the places inside AND branches before
-- and after the subworkflow.
data ANDBranchLabels = ANDBranchLabels
  { inLabel  :: PEditLabel
  , outLabel :: PEditLabel
  } deriving (Eq, Ord, Show)

-- TODO: add smart constructors
-- | `Continuation`s are used to replace holes in `EditableSW`s.
data Continuation
  = Atom { atomLabel       :: TEditLabel   -- ^ Label to be put on the transition
         }
  | AND  { andSplitLabel    :: TEditLabel            -- ^ Label to be put on the splitting transition
         , andJoinLabel     :: TEditLabel            -- ^ Label to be put on the joining transition
         , andBranchLabels  :: List2 ANDBranchLabels -- ^ In and Out annotations of each branch in the AND-split
         }
  -- | XOR-spliting by having an @if@ statement in a single method.
  | IfXOR  { ifXorThenLabel :: TEditLabel   -- ^ Label for the _then_ branch (just for the condition)
           , ifXorElseLabel :: TEditLabel   -- ^ Label for the _else_ branch (just for the condition)
           }
  -- | XOR-spliting by having multiple methods (undetermiinistic semantics).
  | UndetXOR
  | SimpleLoop  { sLoopJumpBackLabel    :: TEditLabel -- ^ Label for the jump-back branch    (just for the condition)
                , sLoopFallThroughLabel :: TEditLabel -- ^ Label for the fall-through branch (just for the condition)
                }
  | Loop { exitLabel            :: PEditLabel            -- ^ Label for the exit place
         , loopBeforeLabel        :: TEditLabel   -- ^ Label for the transition before the breakpoint (just for the condition)
         , loopAfterLabel :: TEditLabel         -- ^ Label for the transition after the breakpoint (exiting from the loop) (just for the condition)
         , loopJumpBackLabel    :: TEditLabel    -- ^ Label for the jump-back branch (just for the condition)
         }
  | Seq  { inbetweenLabel :: PEditLabel -- ^ Label for the place inbetween the two transitions
         }
  -- NOTE: Completely incomplete
  | ACF
  deriving (Eq, Ord, Show)

noLoc :: a -> Located a
noLoc = Located NoLoc

guardedBy :: TEditLabel -> Expr -> TEditLabel
-- guardedBy lbl _
--   | Just code <- cgmCode . trCGMetadata $ lbl
--   = panic $ "guardedBy: Label already contains code (no code displayed means NoOp): " <> prettyPrint code
guardedBy lbl@TEL{..} newCond
  | CGMetadata{..} <- trCGMetadata
  = case cgmIfCond of
    Nothing -> lbl { trCGMetadata = trCGMetadata { cgmIfCond = Just newCond } }
    Just origCond -> let combinedCond = EBinOp (noLoc And) (noLoc origCond) (noLoc newCond)
      in lbl { trCGMetadata = trCGMetadata { cgmIfCond = Just combinedCond } }

mGuardedBy :: TEditLabel -> Maybe Expr -> TEditLabel
mGuardedBy lbl = \case
  Nothing   -> lbl
  Just cond -> lbl `guardedBy` cond

holeWithMCond :: TransId -> Maybe Expr -> EditableSW
holeWithMCond id mCond = SW.Atom $ TEL id (Name $ "_" <> show id) True (CGMetadata Nothing mCond)

-- NOTE: if the label to replaced already has a condition, preserve it
-- TODO: if we ever allow reediting, we will need a smarter way to index holes
fromContinuation
  :: TEditLabel                       -- ^ Label for the transition being replaced
  -> (TransId -> TransId -> TransId) -- ^ Make a new index from the parent and the current one
  -> TransId                       -- ^ Parent index
  -> Continuation                 -- ^ Construct to replace the given transition with
  -> EditableSW
fromContinuation (cgmIfCond.trCGMetadata -> mCond) mkIx parent = \case
  Atom{..}   -> SW.Atom (atomLabel `mGuardedBy` mCond)
  AND{..}    -> SW.AND (andSplitLabel `mGuardedBy` mCond) andJoinLabel $ mkBranches andBranchLabels
  SimpleLoop{..} -> SW.SimpleLoop
    (SW.Atom $ sLoopJumpBackLabel    `mGuardedBy` mCond)
    (SW.Atom $ sLoopFallThroughLabel `mGuardedBy` mCond)
  -- TODO: check whether the labelling of the fall-through and jump-back branches are correct
  Loop{..} -> SW.Loop
    exitLabel
    (SW.Atom $ loopBeforeLabel `mGuardedBy` mCond)
    (SW.Atom $ loopAfterLabel)
    (SW.Atom $ loopJumpBackLabel)
  -- TODO: finish this
  IfXOR{..} -> do
    let thenLabel = ifXorThenLabel `mGuardedBy` mCond
        elseLabel = ifXorElseLabel `mGuardedBy` mCond
    SW.XOR (SW.Atom thenLabel) (SW.Atom elseLabel)
  UndetXOR  -> SW.XOR (holeWithMCond (mkIx' 1) mCond) (holeWithMCond (mkIx' 2) mCond)
  Seq{..}    -> SW.Seq
    inbetweenLabel
    (holeWithMCond (mkIx' 1) mCond)
    (Hole $ mkIx' 2)
  {- TODO: ACF Continuation

     Handling this will probably require place annotations.
     The user could select a place and we would display which other places
     it can be conencted to.

     Implementation plan:
      1. [DONE] Add place annotations to SafeWorkflows
      2. Implement a function that given a place inside an ACF,
         finds all the other places it can be connected to (inside said ACF).
      3. Implement a function that connects two places in an ACF.
      4. Probably will need a function that "merges" ACFs.
         This will be needed to simplify the structure and coalesce
         nested ACFs into a single one.
         Example use case: See the n-ary XOR above. With the current
         implementation we cannot connect places in different branches,
         because they are inside different ACFs.
  -}
  ACF -> panic "not implemented"

  where
    mkIx' :: Int -> TransId
    mkIx' = mkIx parent

    mkBranch :: TransId -> ANDBranchLabels -> ANDBranch PEditLabel TEditLabel
    mkBranch holeId ANDBranchLabels{..} = ANDBranch inLabel outLabel (Hole holeId)

    mkBranches :: List2 ANDBranchLabels -> List2 (ANDBranch PEditLabel TEditLabel)
    mkBranches = GHC.fromList . zipWith mkBranch [1..] . GHC.toList

countHoles :: EditableSW -> Int
countHoles = \case
  sw@(Hole _)               -> 1
  sw@(SW.Atom _)            -> 0
  sw@SW.AND{..}             -> sum $ fmap (countHoles . branchWorkflow) andBranches
  (SW.Loop _ into exit out) -> sum $ map countHoles [into, exit, out]
  (SW.SimpleLoop exit body) -> sum $ map countHoles [exit, body]
  sw@(SW.ACF _ acfMap)      -> sum $ M.map (sum . fmap countHoles) acfMap

replaceHole :: TransId -> Continuation -> EditableSW -> EditableSW
replaceHole holeId cont esw
  | countHoles esw > 1 = replaceHoleWithIndexing mkIxWithPrefix holeId cont esw
  | otherwise          = replaceHoleWithIndexing (\_ x -> x)    holeId cont esw
  where
    mkIxWithPrefix :: TransId -> TransId -> TransId
    mkIxWithPrefix parent ix = 10*parent + ix

-- TODO: implement "condition push-down"
replaceHoleWithIndexing
  :: (TransId -> TransId -> TransId) -- ^ Make a new index from the parent and the current one
  -> TransId                       -- ^ Look for a hole with this identifier
  -> Continuation                 -- ^ Construct to fill the hole with
  -> EditableSW                   -- ^ The workflow to replace the hole in
  -> EditableSW
replaceHoleWithIndexing mkIx holeId cont = \case
  sw@(SW.Atom lbl@TEL{..})
    | trId == holeId && trIsEditable -> fromContinuation lbl mkIx trId cont
  -- TODO: | trId == holeId && not trIsEditable -> panic "..."
    | otherwise -> sw
  sw@SW.AND{..} ->
    sw { andBranches = (mapANDBranchWF $ replaceHoleWithIndexing mkIx holeId cont) <$> andBranches }
  (SW.GenLoop mExitLabel into exit out) ->
    SW.GenLoop mExitLabel
               (replaceHoleWithIndexing mkIx holeId cont <$> into)
               (replaceHoleWithIndexing mkIx holeId cont exit)
               (replaceHoleWithIndexing mkIx holeId cont out)
  sw@(SW.ACF annots acfMap) ->
    unsafeMkACF annots $ M.map (fmap $ replaceHoleWithIndexing mkIx holeId cont) acfMap

pprTrsId :: Int -> Doc
pprTrsId id = "__trans__" <> ppr id

-- NOTE: Didn't want to generalize this type classs any further,
-- so I just added a specialized version of `renderToGraphviz`.
instance DisplayableWorkflow PrettyEditableSW where
  -- | Annotated transition.
  data AnnotatedTransition PrettyEditableSW
    = AnnTr { getRenderingId :: Int
            , getAnnTr       :: SW.AnnTransition PrettyTLabel
            }

  renderTransitionNode :: GV.AnnotatedTransition PrettyEditableSW -> Graphviz
  renderTransitionNode (AnnTr id (SW.AnnTransition ann _)) = prettyPrint $ mconcat
    [ pprTrsId id
    , "[label=<"
    , "<FONT POINT-SIZE=\"16\">" <> ppr ann <> "</FONT>"
    , "<FONT POINT-SIZE=\"10\" COLOR=\"blue\"> "
    , "</FONT>"
    , ">"
    , "shape=box; fontname=\"Arial\"; style=filled; color=black; fillcolor=gray75;]"
    ]

  renderTransitionArrows :: GV.AnnotatedTransition PrettyEditableSW -> Graphviz
  renderTransitionArrows (AnnTr id (SW.AnnTransition ann (Arrow src dst))) = prettyPrint $ hsep
    [ ppr src
    , "->"
    , pprTrsId id
    , ";"
    , pprTrsId id
    , "->"
    , ppr dst
    ]

  annotatedTransitions :: PrettyEditableSW -> [GV.AnnotatedTransition PrettyEditableSW]
  annotatedTransitions = map (uncurry AnnTr)
                       . zip [0..]
                       . getTransitions
                       . constructAnnTransitionsWithoutPlaces
                       . first (const ())

  staticWorkflowStates :: PrettyEditableSW -> Set WorkflowState
  staticWorkflowStates = inferStaticWorkflowStates
                       . constructTransitionsWithoutPlaces
                       . bimap (const ()) (const ())

  renderToGraphviz :: PrettyEditableSW -> Graphviz
  renderToGraphviz wf = digraph $ unlines
    [ options
    , graphvizPlaces
    , graphvizTransitions
    , graphvizArrows
    , graphvizRanks
    ]
    where
      graphvizPlaces :: Graphviz
      graphvizPlaces = unlines
                     . map (uncurry mkAnnotPlace)
                     . M.toList
                     $ placeAnnotMap

      graphvizTransitions :: Graphviz
      graphvizTransitions = unlines
                          . map renderTransitionNode
                          . annotatedTransitions
                          $ wf

      graphvizArrows :: Graphviz
      graphvizArrows = unlines
                     . map renderTransitionArrows
                     . annotatedTransitions
                     $ wf

      -- make ranks to hopefully bring some sanity to the layout of AND-splits
      graphvizRanks :: Graphviz
      graphvizRanks = unlines
                    . mapMaybe mkRank
                    . S.toList
                    . staticWorkflowStates
                    $ wf

      -- | Annotated places (initial and terminal are NOT in this)
      placeAnnotMap :: Map Place PrettyPLabel
      placeAnnotMap = getPlaceAnnotations
                    . constructAnnTransitions (PrettyPLabel LInitial) (PrettyPLabel LTerminal)
                    $ wf

      mkAnnotPlace :: Place -> PrettyPLabel -> Graphviz
      mkAnnotPlace p@(Place _) lbl = prettyPrint . mconcat $
        [ ppr p  -- this is the ID
        , "[label=<"
        , "<FONT POINT-SIZE=\"16\">" <> ppr lbl <> "</FONT>"  -- this is the rendered label
        , "<FONT POINT-SIZE=\"10\" COLOR=\"blue\"> "
        , "</FONT>"
        , ">"
        , " shape=ellipse; fontname=\"Arial\"; fontsize=16; style=filled; color=black; fillcolor=white;]"
        ]
      mkAnnotPlace p@(PlaceEnd) _ = prettyPrint p
        <> " [shape=point; width=0.3; peripheries=2; style=filled; color=\"#d11010\"; label=\"\"]"
      mkAnnotPlace p@(PlaceStart) _ = prettyPrint p
        <> " [shape=point; width=0.3; style=filled; color=\"#0e64ce\"; label=\"\"]"
