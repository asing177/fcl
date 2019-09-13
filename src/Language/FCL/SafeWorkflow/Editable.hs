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

  , prettify
  )
  where

import Protolude

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
import Language.FCL.AST (Name(..), Transition(..), WorkflowState(..), Place(..), Expr(..))
import Language.FCL.Pretty (Doc, Pretty, ppr, hsep, prettyPrint)
import Language.FCL.Analysis (inferStaticWorkflowStates)
import Language.FCL.Graphviz hiding (AnnotatedTransition)
import Language.FCL.SafeWorkflow.Simple (constructTransitionsWithoutPlaces, constructAnnTransitionsWithoutPlaces)

import qualified Language.FCL.SafeWorkflow as SW
import qualified Language.FCL.Graphviz     as GV

-- NOTE: Reediting an already finished transition could be done by
-- transforming it back to a Hole first, then editing it.

-- | Identifier of a transition in an editable workflow
type TransId = Int

-- TODO: See Language.FCL.SafeWorkflow.Codegen/MethodIndentifiers
-- NOTE: global would be for methods (e.g.: preconditions)
-- | Local transition metadata for code generation
data CGMetadata = CGMetadata
  { cgmCode   :: Maybe Expr   -- ^ Code to be generated into the transition (`Nothing` -> `ENoOp`)
  , cgmIfCond :: Maybe Expr   -- ^ Possible @If@ condition for deterministic branhcing (NOTE: the conditions in a given method should always be mutually exclusive and should always cover the entire event-space)
  }
  deriving (Eq, Ord, Show)

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
