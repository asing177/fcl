{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Script.Warning where

import Protolude hiding ((<>))

import qualified Data.Aeson as A
import Script (Name)
import Script.Pretty (Pretty(..), (<>), (<+>), ppr, squotes, vsep)

data Warning
  = UnusedVarWarn Name
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

instance Pretty [Warning] where
  ppr [] = "No warnings."
  ppr ws@(_:_) = vsep $ "Warnings:" : map (("•" <+>) . ppr) ws

instance Pretty Warning where
  ppr = \case
    UnusedVarWarn v -> "Unused variable" <+> squotes (ppr v) <> "."
