module TestSwagger where

import Protolude

import Servant.Swagger.Test
import Test.Hspec
import Test.Hspec.QuickCheck
import HTTP.FCL.API

swaggerTest :: Spec
swaggerTest = modifyMaxSuccess (\_ -> 50)
  $ context "ToJSON matches ToSchema" $ validateEveryToJSON fclProxyAPI
