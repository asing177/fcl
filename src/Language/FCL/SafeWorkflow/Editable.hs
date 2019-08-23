{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
module Language.FCL.SafeWorkflow.Editable
  ( Continuation(..)
  , EditLabel(..)
  , PrettyLabel(..)
  , EditableSW
  , TransId
  , HoleId

  , pattern Hole

  , replaceHole
  )
  where

import Protolude

import GHC.Exts (IsList(..))
import qualified GHC.Show (show)

import qualified Data.Map as M

import Control.Monad.Gen

import Language.FCL.SafeWorkflow hiding
  ( Atom
  , AND
  , pattern SimpleLoop
  , pattern Loop
  , pattern XOR
  , pattern Seq
  , pattern ACF
  )
import Language.FCL.AST (Name(..), Transition(..), WorkflowState)
import Language.FCL.Pretty (Doc, Pretty, ppr, hsep, prettyPrint)
import Language.FCL.Analysis (inferStaticWorkflowStates)
import Language.FCL.Graphviz hiding (AnnotatedTransition)

import qualified Language.FCL.SafeWorkflow as SW
import qualified Language.FCL.Graphviz     as GV

-- NOTE: Reeediting an already finished transition could be done by
-- transforming it back to a Hole first, then editing it.

-- | Identifier of a hole in an editable workflow.
type HoleId = Int

-- | Identifier of a transition in an editable workflow
type TransId = Int

-- | Labels for safe workflow editing.
data EditLabel
  = HLabel HoleId       -- ^ Label for holes
  | TLabel Name TransId -- ^ Label for finished transitions
  deriving (Eq, Ord, Show)

-- | Newtype wrapper for pretty pritning `EditLabel`s
newtype PrettyLabel = PrettyLabel EditLabel
  deriving (Eq, Ord)

instance Pretty PrettyLabel where
  ppr = \case
    PrettyLabel (TLabel name id) -> ppr name
    PrettyLabel (HLabel      id) -> "_" <> ppr id

instance Show PrettyLabel where
  show = show . ppr

-- | Safe workflow enriched with additional annotations
-- to faciliate the editing process.
type EditableSW = SafeWorkflow EditLabel

-- | Safe workflow enriched with additional annotations
-- to faciliate the editing process. This is the same as
-- `EditableSW` just with prettier labels.
type PrettyEditableSW = SafeWorkflow PrettyLabel

prettify :: EditableSW -> PrettyEditableSW
prettify = fmap PrettyLabel

-- | A `Hole` is an `Atom` annotated with a hole label.
-- These are the plugin points of the workflows, this where
-- the workflow can be edited.
pattern Hole :: Int -> EditableSW
pattern Hole n = SW.Atom (HLabel n)

-- | `Continuation`s are used to replace holes in `EditableSW`s.
data Continuation
  = Atom { atomLabel       :: EditLabel   -- ^ Label to be put on the transition
         }
  | AND  { andSplitLabel   :: EditLabel   -- ^ Label to be put on the splitting transition
         , andJoinLabel    :: EditLabel   -- ^ Label to be put on the joining transition
         , andNumBranches  :: Int         -- ^ Number of branches in the AND-split
         }
  | XOR  { xorNumBranches  :: Int         -- ^ Number of branches in the XOR-split
         }
  | SimpleLoop
  | Loop
  | Seq
  | ACF
  deriving (Eq, Ord, Show)

fromContinuation
  :: (HoleId -> HoleId -> HoleId) -- ^ Make a new index from the parent and the current one
  -> HoleId                       -- ^ Parent index
  -> Continuation                 -- ^ Construct to fill the hole with
  -> EditableSW
fromContinuation mkIx parent = \case
  Atom{..}   -> SW.Atom atomLabel
  AND{..}    -> SW.AND andSplitLabel andJoinLabel $ fromList $ map (Hole . mkIx') [1..andNumBranches]
  SimpleLoop -> SW.SimpleLoop (Hole $ mkIx' 1) (Hole $ mkIx' 2)
  Loop       -> SW.Loop (Hole $ mkIx' 1) (Hole $ mkIx' 2) (Hole $ mkIx' 3)
  XOR{..}    -> foldl SW.XOR (Hole $ mkIx' 1) $ map (Hole . mkIx') [2..xorNumBranches]
  Seq        -> SW.Seq (Hole $ mkIx' 1) (Hole $ mkIx' 2)
  {- TODO: ACF Continuation

     Handling this will probably require place annotations.
     The user could select a place and we would display which other places
     it can be conencted to.

     Implementation plan:
      1. Add place annotations to SafeWorkflows
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

  where mkIx' = mkIx parent

countHoles :: EditableSW -> Int
countHoles = \case
  sw@(Hole found)           -> 1
  sw@(SW.Atom _)            -> 0
  sw@SW.AND{..}             -> sum $ fmap countHoles andBranches
  (SW.Loop into exit out)   -> sum $ map countHoles [into, exit, out]
  (SW.SimpleLoop exit body) -> sum $ map countHoles [exit, body]
  sw@(SW.ACF acfMap)        -> sum $ M.map (sum . fmap countHoles) acfMap

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
    sw { andBranches = fmap (replaceHoleWithIndexing mkIx holeId cont) andBranches }
  (SW.GenLoop into exit out) ->
    SW.GenLoop (replaceHoleWithIndexing mkIx holeId cont <$> into)
               (replaceHoleWithIndexing mkIx holeId cont exit)
               (replaceHoleWithIndexing mkIx holeId cont out)
  sw@(SW.ACF acfMap) ->
    unsafeMkACF $ M.map (fmap $ replaceHoleWithIndexing mkIx holeId cont) acfMap

pprTrsId :: Int -> Doc
pprTrsId id = "__trans__" <> ppr id

instance DisplayableWorkflow PrettyEditableSW where
  -- | Annotated transition.
  data AnnotatedTransition PrettyEditableSW
    = AnnTr { getRenderingId :: Int
            , getAnnTr       :: SW.AnnotatedTransition PrettyLabel
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
                       . constructAnnTransitions

  staticWorkflowStates :: PrettyEditableSW -> Set WorkflowState
  staticWorkflowStates = inferStaticWorkflowStates
                       . constructTransitions
