{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
module Language.FCL.SafeWorkflow.Editable
  ( Continuation(..)
  , EditLabel(..)
  , PrettyLabel(..)
  , EditableSW
  , pattern Hole
  , fromContinuation
  , replaceHole
  , nameUnlabelledTransitions
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
  | NoLabel             -- TODO: revisit this
  deriving (Eq, Ord, Show)

-- | Newtype wrapper for pretty pritning `EditLabel`s
newtype PrettyLabel = PrettyLabel EditLabel
  deriving (Eq, Ord)

instance Pretty PrettyLabel where
  ppr = \case
    -- TODO: revisit naming
    PrettyLabel (TLabel name id) -> ppr name
    PrettyLabel (HLabel      id) -> "_" <> ppr id
    PrettyLabel NoLabel          -> "no_label"

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

-- | Constructs to replace a hole with in an editable workflow.
data Continuation
  = Atom
  | AND Int
  | SimpleLoop
  | Loop
  | XOR Int
  | Seq
  | ACF
  deriving (Eq, Ord, Show)

fromContinuation
  :: (HoleId -> HoleId -> HoleId) -- ^ Make a new index from the parent and the current one
  -> HoleId                       -- ^ Parent index
  -> Continuation                 -- ^ Construct to fill the hole with
  -> EditableSW
fromContinuation mkIx parent = \case
  Atom       -> SW.Atom NoLabel
  (AND n)    -> SW.AND NoLabel NoLabel $ fromList $ map (Hole . mkIx') [1..n]
  SimpleLoop -> SW.SimpleLoop (Hole $ mkIx' 1) (Hole $ mkIx' 2)
  Loop       -> SW.Loop (Hole $ mkIx' 1) (Hole $ mkIx' 2) (Hole $ mkIx' 3)
  (XOR n)    -> foldl SW.XOR (Hole $ mkIx' 1) $ map (Hole . mkIx') [2..n]
  Seq        -> SW.Seq (Hole $ mkIx' 1) (Hole $ mkIx' 2)
  ACF        -> panic "not implemented"

  where mkIx' = mkIx parent

countHoles :: EditableSW -> Int
countHoles = \case
  sw@(Hole found)           -> 1
  sw@(SW.Atom _)            -> 0
  sw@SW.AND{..}             -> sum $ fmap countHoles andBranches
  (SW.Loop into exit out)   -> sum $ map countHoles [into, exit, out]
  (SW.SimpleLoop exit body) -> sum $ map countHoles [exit, body]
  sw@(SW.ACF acfMap)        -> sum $ M.map (sum . fmap countHoles) acfMap

replaceHole :: EditLabel -> Continuation -> EditableSW -> EditableSW
replaceHole lbl cont esw
  | countHoles esw > 1 = replaceHoleWithIndexing mkIxWithPrefix lbl cont esw
  | otherwise          = replaceHoleWithIndexing (\_ x -> x)    lbl cont esw
  where
    mkIxWithPrefix :: HoleId -> HoleId -> HoleId
    mkIxWithPrefix parent ix = 10*parent + ix

replaceHoleWithIndexing
  :: (HoleId -> HoleId -> HoleId) -- ^ Make a new index from the parent and the current one
  -> EditLabel                    -- ^ Look for a hole with this label
  -> Continuation                 -- ^ Construct to fill the hole with
  -> EditableSW                   -- ^ The workflow to replace the hole in
  -> EditableSW
replaceHoleWithIndexing mkIx lbl@(HLabel n) cont = \case
  sw@(Hole found)
    | n == found -> fromContinuation mkIx found cont
    | otherwise -> sw
  sw@(SW.Atom _) -> sw
  sw@SW.AND{..} ->
    sw { andBranches = fmap (replaceHoleWithIndexing mkIx lbl cont) andBranches }
  (SW.GenLoop into exit out) ->
    SW.GenLoop (replaceHoleWithIndexing mkIx lbl cont <$> into)
               (replaceHoleWithIndexing mkIx lbl cont exit)
               (replaceHoleWithIndexing mkIx lbl cont out)
  sw@(SW.ACF acfMap) ->
    unsafeMkACF $ M.map (fmap $ replaceHoleWithIndexing mkIx lbl cont) acfMap
replaceHoleWithIndexing mkIx lbl _ = panic $ "replaceHoleWithIndexing: Didn't get a hole label: " <> show lbl

-- TODO: make the generation of transition names and IDs more general
nameUnlabelledTransitions :: EditableSW -> EditableSW
nameUnlabelledTransitions esw = runGen $ forM esw $ \case
  NoLabel -> do
    id <- gen
    pure $ TLabel (Name $ "T" <> show id) id
  lbl -> pure lbl

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
