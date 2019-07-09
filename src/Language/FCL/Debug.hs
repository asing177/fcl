module Language.FCL.Debug where

import Protolude

import qualified Data.Set as S

import Language.FCL.AST
import Language.FCL.Pretty

import qualified Language.FCL.Token as Token


-- | Wrapper for debug printing.
newtype Debug a = Debug a
  deriving (Eq, Ord)

instance Pretty (Debug WorkflowState) where
  ppr (Debug wfs) = setOf . S.toList . places $ wfs

instance Pretty (Debug Transition) where
  ppr (Debug (Arrow from to)) =
    token Token.transition <+> ppr (Debug from) <+> token Token.rarrow <+> ppr (Debug to)

instance Pretty (Debug Place) where
  ppr (Debug p) = ppr p
