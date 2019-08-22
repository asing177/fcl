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

-- | Labels for safe workflow editing
data EditLabel
  = NoLabel    -- TODO: revisit this
  | HLabel Int -- ^ Label for holes
  deriving (Eq, Ord, Show)

-- | Safe workflow enriched with additional annotations
-- to faciliate the editing process.
type EditableSW = SafeWorkflow EditLabel

-- | A `Hole` is an `Atom` annotated with a hole label.
-- These are the plugin points of the workflows, this where
-- the workflow can be edited.
pattern Hole :: Int -> EditableSW
pattern Hole n = SW.Atom (HLabel n)

data Continuation
  = Atom
  | AND Int
  | SimpleLoop
  | Loop
  | XOR Int
  | Seq
  | ACF
  deriving (Eq, Ord, Show)

fromContinuation :: Continuation -> EditableSW
fromContinuation Atom       = SW.Atom NoLabel
fromContinuation (AND n)    = SW.AND NoLabel NoLabel $ fromList $ map Hole [1..n]
fromContinuation SimpleLoop = SW.SimpleLoop (Hole 1) (Hole 2)
fromContinuation Loop       = SW.Loop (Hole 1) (Hole 2) (Hole 2)
fromContinuation (XOR n)    = foldl SW.XOR (Hole 1) $ map Hole [2..n]
fromContinuation Seq        = SW.Seq (Hole 1) (Hole 2)
fromContinuation ACF        = panic "not implemented"

replaceHole :: EditLabel -> Continuation -> EditableSW -> EditableSW
replaceHole lbl@(HLabel n) cont = \case
  sw@(Hole found)
    | n == found -> fromContinuation cont
    | otherwise -> sw
  sw@(SW.Atom _) -> sw
  sw@SW.AND{..} ->
    sw { andBranches = fmap (replaceHole lbl cont) andBranches }
  (SW.GenLoop into exit out) ->
    SW.GenLoop (replaceHole lbl cont <$> into)
               (replaceHole lbl cont exit)
               (replaceHole lbl cont out)
  sw@(SW.ACF acfMap) ->
    unsafeMkACF $ M.map (fmap $ replaceHole lbl cont) acfMap
replaceHole lbl _ = panic $ "replaceHole: Didn't get a hole label: " <> show lbl

