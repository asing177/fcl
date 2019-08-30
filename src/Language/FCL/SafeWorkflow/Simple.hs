{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
module Language.FCL.SafeWorkflow.Simple
  ( SimpleSafeWorkflow
  , pattern SAtom
  , pattern SAND
  , pattern SAND2

  , module Language.FCL.SafeWorkflow
  ) where

import Data.List.List2

import Language.FCL.SafeWorkflow

type SimpleSafeWorkflow = SafeWorkflow ()

pattern SAtom :: SimpleSafeWorkflow
pattern SAtom = Atom ()

pattern SAND :: List2 SimpleSafeWorkflow -> SimpleSafeWorkflow
pattern SAND swfs = AND () () swfs

-- | Simple AND with two branches.
pattern SAND2 :: SimpleSafeWorkflow -> SimpleSafeWorkflow -> SimpleSafeWorkflow
pattern SAND2 lhs rhs <- SAND (AsList [lhs,rhs])
  where SAND2 lhs rhs =  SAND [lhs,rhs]

