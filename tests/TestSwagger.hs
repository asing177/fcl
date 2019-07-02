{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}

module TestSwagger where

import Protolude

import Servant.Swagger.Test
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import HTTP.FCL.API
import Language.FCL.LanguageServerProtocol
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Swagger.Schema.Validation
swaggerTest :: Spec
swaggerTest = modifyMaxSuccess (\_ -> 50)
  $ context "ToJSON matches ToSchema" $ validateEveryToJSON fclProxyAPI

-- apiTests :: TestTree
-- apiTests = testGroup "API swagger tests"

--   [ testProperty "Request Definition" (\(req :: ReqDef) -> (validateToJSON req === []))
--   , testProperty "Request Method" (\(req :: ReqMethod) -> (validateToJSON req === []))
--   , localOption (QuickCheckTests 5)
--     $ testProperty "Request Script" (\(req :: ReqScript) -> (validateToJSON req === []))
--   , localOption (QuickCheckTests 1)
--     $ testProperty "Response Definition" (\(resp :: RespDef) -> traceShow resp (validateToJSON resp === []))
--   , localOption (QuickCheckTests 5)
--     $ testProperty "Response Method" (\(resp :: RespMethod) -> (validateToJSON resp === []))
--   , localOption (QuickCheckTests 5)
--     $ testProperty "Response Script" (\(resp :: RespScript) -> traceShow resp (validateToJSON resp === []))
--   ]
