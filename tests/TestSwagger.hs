{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestSwagger where

import Protolude

import Servant.Swagger.Test
import Test.Hspec
import Test.QuickCheck
import HTTP.FCL.API

swaggerTest :: Spec
swaggerTest = describe "Swagger" $ do
  context "ToJSON matches ToSchema" $ validateEveryToJSON fclProxyAPI
