{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestSwagger where

import Protolude

import Servant.Swagger.Test
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import HTTP.FCL.API

swaggerTest :: Spec
swaggerTest = modifyMaxSuccess (\_ -> 5)
  $ context "ToJSON matches ToSchema" $ validateEveryToJSON fclProxyAPI
