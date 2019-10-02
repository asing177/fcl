{-# options_ghc -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}


module Language.FCL.Orphans () where

import Protolude hiding (Hashable, put, get)
import Prelude (read)

import Control.Monad.Fail (MonadFail(..))
import Crypto.Random (SystemDRG)
import Crypto.Random.Types (MonadPseudoRandom)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Number.CReal (CReal)
import Data.Serialize (Serialize(..))
import qualified Data.Hourglass as DH
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Serialize as Serialize
import qualified Data.Time.Calendar as DC
import qualified Datetime.Types as DT
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary(..))

-- import Language.FCL.Hash (Hashable(..))
import Language.FCL.Pretty (Pretty(..), text)

--------------------------------------------------------------------------------
-- JSON
--------------------------------------------------------------------------------

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8

instance FromJSON ByteString where
  parseJSON v = do
    t :: Text <- parseJSON v
    pure $ encodeUtf8 t

--------------------------------------------------------------------------------
-- MonadFail
--------------------------------------------------------------------------------

type RandomM = MonadPseudoRandom SystemDRG

-- TODO: Fix the MonadFail issues
instance MonadFail RandomM where
  fail s = panic ("Monad RandomM Fail " <> toS s)

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

instance Serialize a => Serialize (NonEmpty a) where
  get = NonEmpty.fromList <$> Serialize.get
  put = Serialize.put . NonEmpty.toList

--------------------------------------------------------------------------------
-- QuickCheck
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
  shrink = QC.genericShrink

instance Arbitrary DT.Datetime where
  arbitrary = DT.posixToDatetime <$> QC.choose (1, 32503680000) -- (01/01/1970, 01/01/3000)

instance Arbitrary DT.Period where
  arbitrary = do
    year <- QC.choose (0,1000)
    month <- QC.choose (0,12)
    let monthNumDays = DC.gregorianMonthLength (fromIntegral year) month
    day <- QC.choose (0, monthNumDays)
    pure $ DT.Period $ DH.Period year month day

instance Arbitrary DT.Duration where
  arbitrary = fmap DT.Duration $ DH.Duration
    <$> (DH.Hours <$> QC.choose (0,23))
    <*> (DH.Minutes <$> QC.choose (0,59))
    <*> (DH.Seconds <$> QC.choose (0,59))
    <*> pure 0

instance Arbitrary DT.Delta where
  arbitrary = DT.Delta <$> arbitrary <*> arbitrary

--------------------------------------------------------------------------------
-- Instances by data type
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- CReal
--------------------------------------------------------------------------------

instance Serialize CReal where
  put = undefined
  get = undefined

instance ToJSON CReal where
  toJSON r = toJSON @Text $ show r

instance FromJSON CReal where
  parseJSON v = read . toS <$> parseJSON @Text v

instance Pretty CReal where
  ppr = text . show

instance Arbitrary CReal where
  arbitrary = realToFrac <$> arbitrary @Rational
