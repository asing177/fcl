{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
module Language.FCL.SafeWorkflow.DSL
  ( Continuation(..)
  , HLabel
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
-- TODO: generalize annotation
import Language.FCL.SafeWorkflow.Simple (SimpleSafeWorkflow)

data Continuation
  = Atom
  | AND Int
  | SimpleLoop
  | Loop
  | XOR Int
  | Seq
  | ACF
  deriving (Eq, Ord, Show)

-- TODO: generalize unit annotation with free type variable

fromContinuation :: Continuation -> SimpleSafeWorkflow
fromContinuation Atom       = SW.Atom ()
fromContinuation (AND n)    = SW.AND () () $ fromList $ map SW.Hole [1..n]
fromContinuation SimpleLoop = SW.SimpleLoop (SW.Hole 1) (SW.Hole 2)
fromContinuation Loop       = SW.Loop (SW.Hole 1) (SW.Hole 2) (SW.Hole 2)
fromContinuation (XOR n)    = foldl SW.XOR (SW.Hole 1) $ map SW.Hole [2..n]
fromContinuation Seq        = SW.Seq (SW.Hole 1) (SW.Hole 2)
fromContinuation ACF        = panic "not implemented"

replaceHole :: HLabel -> Continuation -> SimpleSafeWorkflow -> SimpleSafeWorkflow
replaceHole lbl cont = \case
  sw@(SW.Hole foundLbl)
    | lbl == foundLbl -> fromContinuation cont
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

