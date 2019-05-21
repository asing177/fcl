{-|

Metadata data structures and serialization.

--}


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Language.FCL.Metadata (
  Metadata(..),
) where

import Protolude
import qualified Language.FCL.Utils as Utils

import qualified Data.Map as Map
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Binary as B
import qualified Data.Serialize as S
import qualified Data.Serialize.Text()
import qualified Data.ByteString as BS

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Metadata = Metadata
  { unMetadata :: Map Text Text }
  deriving (Show, Eq, Generic)

instance Semigroup Metadata where
  (Metadata m1) <> (Metadata m2) =
    Metadata $ m1 <> m2

instance Monoid Metadata where
  mempty = Metadata mempty

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance A.ToJSON Metadata where
  toJSON (Metadata md) = A.toJSON md

instance A.FromJSON Metadata where
  parseJSON v =
    case v of
      A.Object _ ->
        Metadata <$> A.parseJSON v
      invalid    ->
        A.typeMismatch "Metadata" invalid

instance S.Serialize Metadata where
  put (Metadata m) = do
    let len = Map.size m
    S.putWord16be $ Utils.toWord16 len
    go $ sortBy (\a b -> compare (fst a) (fst b)) $
      map (bimap toS toS) $ Map.toList m

    where
      go [] = return ()
      go ((k,v):xs) = do
        S.putWord16be $ Utils.toWord16 $ BS.length k
        S.putByteString k
        S.putWord16be $ Utils.toWord16 $ BS.length v
        S.putByteString v
        go xs
  get = do
      len <- S.getWord16be
      go [] 0 len
    where
      go acc i len
        | i == len = return $ Metadata $ Map.fromList acc
        | otherwise = do
          keyLen <- S.getWord16be
          key    <- S.getBytes $ Utils.toInt keyLen
          valLen <- S.getWord16be
          val    <- S.getBytes $ Utils.toInt valLen
          go ((toS key, toS val) : acc) (i+1) len

instance B.Binary Metadata where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize
