{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
module Language.FCL.SafeWorkflow.Editable
  ( Continuation(..)
  , TEditLabel(..)
  , PEditLabel(..)
  , PrettyTLabel(..)
  , PrettyPLabel(..)
  , Condition(..)
  , EditableSW
  , PlaceId
  , TransId
  , HoleId
  , ANDBranchLabels(..)

  , pattern Hole

  , replaceHole
  , prettify
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
import Language.FCL.AST (Name(..), Transition(..), WorkflowState(..), Place(..), Expr)
import Language.FCL.Pretty (Doc, Pretty, ppr, hsep, prettyPrint)
import Language.FCL.Analysis (inferStaticWorkflowStates)
import Language.FCL.Graphviz hiding (AnnotatedTransition)
import Language.FCL.SafeWorkflow.Simple (constructTransitionsWithoutPlaces, constructAnnTransitionsWithoutPlaces)

import qualified Language.FCL.SafeWorkflow as SW
import qualified Language.FCL.Graphviz     as GV

-- TODO: Separate metadata (name) from the datastructure.
-- Only store IDs and maintain a metadata table.

-- NOTE: Reeediting an already finished transition could be done by
-- transforming it back to a Hole first, then editing it.

-- | Identifier of a hole in an editable workflow.
type HoleId = Int

-- | Identifier of a transition in an editable workflow
type TransId = Int

-- | Transition labels for safe workflow editing.
data TEditLabel
  -- | Label for holes
  = LHole { holeId :: HoleId          -- ^ Hole identifier
          }
  -- | Label for simple finished transitions
  | LSimple { simpleName :: Name      -- ^ Transition name
            , simpleId   :: TransId   -- ^ Transition identifier
            }
  -- | Label for a transition inside an @If@ condition
  | LIfCond { ifcName :: Name         -- ^ Transition name
            , ifcId   :: TransId      -- ^ Transition identifier
            , ifcCond :: Condition    -- ^ Condition for this transition to be activated
            }
  deriving (Eq, Ord, Show)

data Condition
  = CExpr { condExpr :: Expr  -- ^ AST of the If condition
          , condLvl  :: Int   -- ^ Level of nesting (hihger integer -> deeper nesting)
          }
  | CDefault
  deriving (Eq, Show)

instance Ord Condition where
  (<=) CExpr{} CDefault          = True
  (<=) CDefault CExpr{}          = False
  (<=) CDefault CDefault         = True
  (<=) (CExpr _ l1) (CExpr _ l2) = l1 <= l2

-- | Newtype wrapper for pretty pritning `TEditLabel`s
newtype PrettyTLabel = PrettyTLabel TEditLabel
  deriving (Eq, Ord)

instance Pretty PrettyTLabel where
  ppr = \case
    PrettyTLabel (LHole id) -> "_" <> ppr id
    PrettyTLabel (LSimple name _) -> ppr name
    PrettyTLabel (LIfCond name _ _) -> ppr name

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
pattern Hole :: Int -> EditableSW
pattern Hole n = SW.Atom (LHole n)

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
  | XOR  { xorNumBranches  :: Int         -- ^ Number of branches in the XOR-split
         }
  | SimpleLoop
  | Loop { exitLabel :: PEditLabel -- ^ Label for the exit place
         }
  | Seq  { inbetweenLabel :: PEditLabel -- ^ Label for the palce inbetween the two transitions
         }
  | ACF
  deriving (Eq, Ord, Show)

fromContinuation
  :: (HoleId -> HoleId -> HoleId) -- ^ Make a new index from the parent and the current one
  -> HoleId                       -- ^ Parent index
  -> Continuation                 -- ^ Construct to fill the hole with
  -> EditableSW
fromContinuation mkIx parent = \case
  Atom{..}   -> SW.Atom atomLabel
  AND{..}    -> SW.AND andSplitLabel andJoinLabel $ mkBranches andBranchLabels
  SimpleLoop -> SW.SimpleLoop (Hole $ mkIx' 1) (Hole $ mkIx' 2)
  Loop{..}   -> SW.Loop exitLabel (Hole $ mkIx' 1) (Hole $ mkIx' 2) (Hole $ mkIx' 3)
  XOR{..}    -> foldl SW.XOR (Hole $ mkIx' 1) $ map (Hole . mkIx') [2..xorNumBranches]
  Seq{..}    -> SW.Seq inbetweenLabel (Hole $ mkIx' 1) (Hole $ mkIx' 2)
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
    mkIx' :: Int -> HoleId
    mkIx' = mkIx parent

    mkBranch :: HoleId -> ANDBranchLabels -> ANDBranch PEditLabel TEditLabel
    mkBranch holeId ANDBranchLabels{..} = ANDBranch inLabel outLabel (Hole holeId)

    mkBranches :: List2 ANDBranchLabels -> List2 (ANDBranch PEditLabel TEditLabel)
    mkBranches = GHC.fromList . zipWith mkBranch [1..] . GHC.toList

countHoles :: EditableSW -> Int
countHoles = \case
  sw@(Hole found)           -> 1
  sw@(SW.Atom _)            -> 0
  sw@SW.AND{..}             -> sum $ fmap (countHoles . branchWorkflow) andBranches
  (SW.Loop _ into exit out)   -> sum $ map countHoles [into, exit, out]
  (SW.SimpleLoop exit body) -> sum $ map countHoles [exit, body]
  sw@(SW.ACF _ acfMap)        -> sum $ M.map (sum . fmap countHoles) acfMap

replaceHole :: HoleId -> Continuation -> EditableSW -> EditableSW
replaceHole holeId cont esw
  | countHoles esw > 1 = replaceHoleWithIndexing mkIxWithPrefix holeId cont esw
  | otherwise          = replaceHoleWithIndexing (\_ x -> x)    holeId cont esw
  where
    mkIxWithPrefix :: HoleId -> HoleId -> HoleId
    mkIxWithPrefix parent ix = 10*parent + ix

replaceHoleWithIndexing
  :: (HoleId -> HoleId -> HoleId) -- ^ Make a new index from the parent and the current one
  -> HoleId                       -- ^ Look for a hole with this identifier
  -> Continuation                 -- ^ Construct to fill the hole with
  -> EditableSW                   -- ^ The workflow to replace the hole in
  -> EditableSW
replaceHoleWithIndexing mkIx holeId cont = \case
  sw@(Hole found)
    | holeId == found -> fromContinuation mkIx found cont
    | otherwise -> sw
  sw@(SW.Atom _) -> sw
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
