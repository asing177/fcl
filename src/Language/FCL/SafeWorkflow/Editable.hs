{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
module Language.FCL.SafeWorkflow.Editable
  ( Continuation(..)
  , EditLabel(..)
  , PrettyLabel(..)
  , EditableSW
  , pattern Hole
  , fromContinuation
  , replaceHole
  , refreshTransitionIndices
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
import Language.FCL.AST (Name(..))
import Language.FCL.Pretty (Pretty, ppr)

import qualified Language.FCL.SafeWorkflow as SW

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
    PrettyLabel (TLabel name id) -> "T" <> ppr id
    PrettyLabel (HLabel      id) -> "_" <> ppr id
    PrettyLabel NoLabel          -> "no_label"

instance Show PrettyLabel where
  show = show . ppr

-- | Safe workflow enriched with additional annotations
-- to faciliate the editing process.
type EditableSW = SafeWorkflow EditLabel

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

refreshTransitionIndices :: EditableSW -> EditableSW
refreshTransitionIndices esw = runGen $ forM esw $ \case
  -- TODO: revisit this
  NoLabel        -> TLabel ""   <$> gen
  lbl@HLabel{}   -> pure lbl
  TLabel name id -> TLabel name <$> gen
