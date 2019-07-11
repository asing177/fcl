{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Storage (
  storageTests,
) where

import Protolude

import Data.Serialize
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Language.FCL.Time as Time
import Language.FCL.Address
import qualified Test.Reference as Ref
import qualified Language.FCL.Compile as Compile

storageTests :: TestTree
storageTests = testGroup "Storage tests"
  [ HUnit.testCase "Script Serialisation" $ do
      ts <- Time.now
      let script = Ref.testScript
          store  = Ref.testStorage
          addr = Ref.testAddr3
          enc  = runPut $ Compile.putScript script (Just store) addr
          dec  = Compile.readScript enc
      HUnit.assertEqual "" (Right (script, Just store, addr)) dec
  ]
