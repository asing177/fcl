{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
module Language.FCL.SafeWorkflow.Editable
  ( Continuation(..)
  , EditLabel(..)
  , EditableSW
  , pattern Hole
  , fromContinuation
  , replaceHole
  )
  where

import Protolude

import GHC.Exts (IsList(..))

import qualified Data.Map as M

import Language.FCL.SafeWorkflow hiding
  ( Atom
  , AND
  , pattern SimpleLoop
  , pattern Loop
  , pattern XOR
  , pattern Seq
  , pattern ACF
  )
import qualified Language.FCL.SafeWorkflow as SW

-- | Index of a hole in an editable workflow.
type HoleIx = Int

-- | Labels for safe workflow editing.
data EditLabel
  = NoLabel       -- TODO: revisit this
  | HLabel HoleIx -- ^ Label for holes
  deriving (Eq, Ord, Show)

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
  :: (HoleIx -> HoleIx -> HoleIx) -- ^ Make a new index from the parent and the current one
  -> HoleIx                       -- ^ Parent index
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
    mkIxWithPrefix :: HoleIx -> HoleIx -> HoleIx
    mkIxWithPrefix parent ix = 10*parent + ix

replaceHoleWithIndexing
  :: (HoleIx -> HoleIx -> HoleIx) -- ^ Make a new index from the parent and the current one
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
