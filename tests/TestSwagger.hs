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
