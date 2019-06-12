{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestSwagger where

import Protolude

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Servant.Swagger.Test
import           Test.Hspec
import           Test.QuickCheck
import API
import Language.FCL.AST as AST
import Language.FCL.LanguageServerProtocol as LSP
import Language.FCL.Compile as Compile
import Language.FCL.Parser as Parser
import Language.FCL.Duplicate as Dupl
import TestArbitrary ()
import TestNumber ()

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

instance Arbitrary Loc where
  arbitrary = oneof
    [ pure NoLoc
    , Loc <$> arbitrary <*> arbitrary
    ]

instance Arbitrary a => Arbitrary (AST.Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary

instance Arbitrary Dupl.DuplicateError where
  arbitrary = oneof
    [ DuplicateFunction <$> arbitrary
    , DuplicateEnumDef <$> arbitrary
    ]

instance Arbitrary AST.Preconditions where
  arbitrary = AST.Preconditions <$> arbitrary

instance Arbitrary AST.Precondition where
  arbitrary = oneof [ pure PrecAfter, pure PrecBefore, pure PrecRoles ]

instance Arbitrary AST.Expr where
  -- This will suffice for our purposes
  arbitrary = pure AST.ENoOp

instance Arbitrary AST.Method where
  arbitrary = AST.Method <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AST.Arg where
  arbitrary = AST.Arg <$> arbitrary <*> arbitrary

instance Arbitrary AST.EnumDef where
  arbitrary = AST.EnumDef <$> arbitrary <*> pure []

instance Arbitrary AST.Script where
  arbitrary = AST.Script <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary NumPrecision where
  arbitrary = oneof
    [ pure NPArbitrary
    , NPDecimalPlaces <$> arbitrary
    , NPAdd <$> arbitrary <*> arbitrary
    , NPMax <$> arbitrary <*> arbitrary
    ]

instance Arbitrary TCollection where
  arbitrary = oneof
    [ TMap <$> arbitrary <*> arbitrary
    , TSet <$> arbitrary
    ]

instance Arbitrary AST.Type where
  arbitrary = oneof
    [ pure TError
    , TVar <$> arbitrary
    , pure TAny
    , TNum <$> arbitrary
    , pure TBool
    , pure TAccount
    , TAsset <$> arbitrary
    , pure TContract
    , pure TText
    , pure TSig
    , pure TVoid
    , pure TDateTime
    , pure TTimeDelta
    , pure TState
    , TEnum <$> arbitrary
    , TFun <$> arbitrary <*> arbitrary
    , TColl <$> arbitrary
    , pure TTransition
    ]

instance Arbitrary Def where
  arbitrary = oneof
    [ GlobalDef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , GlobalDefNull <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary TVar where
  arbitrary = oneof
    [ TV <$> arbitrary
    , TAV <$> arbitrary
    , TCV <$> arbitrary
    , THV <$> arbitrary
    ]

instance Arbitrary Helper where
  arbitrary = Helper <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Transition where
  arbitrary = Arrow <$> arbitrary <*> arbitrary

instance Arbitrary ReqScript where
  arbitrary = ReqScript <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ReqTransition where
  arbitrary = ReqTransition <$> arbitrary <*> arbitrary

instance Arbitrary ReqMethod where
  arbitrary = ReqMethod <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ReqMethodArg where
  arbitrary = ReqMethodArg <$> arbitrary <*> arbitrary

instance Arbitrary ReqDef where
  arbitrary = oneof
    [ ReqGlobalDef <$> arbitrary <*> arbitrary <*> arbitrary
    , ReqGlobalDefNull <$> arbitrary <*> arbitrary
    ]

instance Arbitrary ReqEnumDef where
  arbitrary = ReqEnumDef <$> arbitrary <*> arbitrary

instance Arbitrary RespMethod where
  arbitrary = RespMethod <$> arbitrary <*> arbitrary

instance Arbitrary RespScript where
  arbitrary = RespScript <$> arbitrary <*> arbitrary <*> pure [] <*> pure [] <*> arbitrary

instance Arbitrary RespDef where
  arbitrary = RespDef <$> arbitrary

instance Arbitrary a => Arbitrary (RPCResponse a) where
  arbitrary = oneof
    [ RPCResp <$> arbitrary
    , RPCRespError <$> arbitrary
    , pure RPCRespOK
    ]

swaggerTest :: Spec
swaggerTest = describe "Swagger" $ do
  context "ToJSON matches ToSchema" $ validateEveryToJSON appAPI
