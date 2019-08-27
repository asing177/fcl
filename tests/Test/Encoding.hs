{-# LANGUAGE TypeApplications #-}

module Test.Encoding (
  encodingTests,
) where

import Protolude
import Test.Tasty
import Test.Tasty.QuickCheck
import Language.FCL.Encoding
import Test.Helpers

encodingTests :: TestTree
encodingTests =
  testGroup "Encoding Round-trip tests"
  [ testProperty "Base58 encoding" $
    \(t :: Text) -> Just (toS t) == (decodeBase58M . encodeBase58 . toS $ t)
  , testProperty "Base58 encoding arbitrary instance" $
    \b58 -> isJust $ decodeBase58M b58
  , testProperty "Base16 encoding" $
    \(t :: Text) -> roundTripProperty encodeBase16 (first toS . decodeBase16E) (toS t)
  , testProperty "Base16 encoding arbitrary instance" $
    \b16 -> isRight $ decodeBase16E b16
  , testProperty "Base64 encoding" $
    \(t :: Text) -> roundTripProperty encodeBase64 (first toS . decodeBase64E) (toS t)
  , testProperty "Base64 encoding arbitrary instance" $
    \b64 -> isRight $ decodeBase64E b64
  , testProperty "Base64P encoding" $
    \(t :: Text) -> roundTripProperty encodeBase64P (first toS . decodeBase64PE) (toS t)
  , testProperty "Base64P encoding arbitrary instance" $
    \b64P -> isRight $ decodeBase64PE b64P
  ]
