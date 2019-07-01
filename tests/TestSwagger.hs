{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestSwagger where

import Protolude

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Servant.Swagger.Test
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Text
import Data.Text
import HTTP.FCL.API
import Language.FCL.AST as AST
import Language.FCL.LanguageServerProtocol as LSP
import Language.FCL.Compile as Compile
import Language.FCL.Parser as Parser
import Language.FCL.Duplicate as Dupl

instance Arbitrary LSPErr where
  arbitrary = LSPErr <$> arbitrary <*> arbitrary

instance Arbitrary RPCResponseError where
  arbitrary = RPCLSPErr <$> arbitrary

instance Arbitrary LSP where
  arbitrary
    = LSP <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Parser.ParseErrInfo where
  arbitrary = Parser.ParseErrInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Compile.CompilationErr where
  arbitrary = oneof
    [ ParseErr <$> arbitrary
    , DuplicationErr <$> arbitrary
    ]

instance Arbitrary Dupl.DuplicateError where
  arbitrary = oneof
    [ DuplicateFunction <$> arbitrary
    , DuplicateADTDef <$> arbitrary
    ]

instance Arbitrary ReqScript where
  arbitrary = ReqScript <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ReqTransition where
  arbitrary = ReqTransition <$> arbitrary <*> arbitrary

instance Arbitrary ReqMethod where
  arbitrary = ReqMethod <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ReqMethodArg where
  arbitrary = ReqMethodArg <$> arbitrary <*> arbitrary

instance Arbitrary ReqDef where
  arbitrary = ReqDef <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ReqADTDef where
  arbitrary = ReqADTDef <$> arbitrary <*> arbitrary

instance Arbitrary RespMethod where
  arbitrary = RespMethod <$> arbitrary <*> arbitrary

instance Arbitrary RespScript where
  arbitrary = RespScript <$> arbitrary <*> arbitrary <*> pure [] <*> pure [] <*> arbitrary

instance Arbitrary RespDef where
  arbitrary = RespDef <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (RPCResponse a) where
  arbitrary = oneof
    [ RPCResp <$> arbitrary
    , RPCRespError <$> arbitrary
    , pure RPCRespOK
    ]

swaggerTest :: Spec
swaggerTest = describe "Swagger" $ do
  context "ToJSON matches ToSchema" $ validateEveryToJSON fclProxyAPI
