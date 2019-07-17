{-# LANGUAGE TypeApplications #-}

module Test.Json (
  jsonTests,
) where

import Protolude

import Data.Aeson as A
import Data.Aeson.Encode.Pretty

import qualified Data.ByteString.Lazy as BSL

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Language.FCL.AST as Script

import Test.Helpers
import qualified Test.Reference as Reference

-- | Test that a JSON representation of the core data structures kept invariant.
jsonTests :: TestTree
jsonTests =
  testGroup "JSON Serialize Tests"
    [ testGroup "JSON Serializer Golden tests"
        -- Structures
        [ goldenVsString "JSON Address"
          addrFile
          (pure (encodePretty Reference.testAddr))

        , goldenVsString "JSON Contract"
          contractFile
          (pure (encodePretty (Reference.testContract Reference.testTimestamp)))

        , goldenVsString "JSON Storage"
          storageFile
          (pure (encodePretty Reference.testStorage))
        ]
    , testGroup "JSON Serializer Round-trip tests"
        [ testCase "JSON Address" $
            roundTripTest A.encode eitherDecodeTextErr Reference.testAddr
        , testCase "JSON Contract" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testContract Reference.testTimestamp
        , testCase "JSON Storage" $
            roundTripTest A.encode eitherDecodeTextErr Reference.testStorage
        , testProperty "JSON Value" $
            roundTripProperty @Script.Value A.encode eitherDecodeTextErr
        ]
    ]

  where
    -- Structures
    addrFile              = "tests/json/address.json"
    acctFile              = "tests/json/account.json"
    contractFile          = "tests/json/contract.json"
    storageFile           = "tests/json/storage.json"

eitherDecodeTextErr :: FromJSON a => BSL.ByteString -> Either Text a
eitherDecodeTextErr = first toSL . A.eitherDecode
