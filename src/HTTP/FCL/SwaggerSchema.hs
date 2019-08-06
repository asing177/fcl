{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances          #-}
module HTTP.FCL.SwaggerSchema () where

import Protolude hiding (get, from, Type)
import Data.Swagger
import Datetime.Types
import Data.HashMap.Strict.InsOrd

import Language.FCL.AST as AST hiding (at)
import Language.FCL.Prim
import Language.FCL.Hash
import Language.FCL.Address
import Language.FCL.Key as Key
import Language.FCL.Time (Timestamp)
import Language.FCL.Asset
import Language.FCL.Contract
import Language.FCL.Parser as Parser
import Language.FCL.Typecheck as Typecheck
import Language.FCL.Compile as Compile
import Language.FCL.Warning as Warning
import Language.FCL.Effect as Effect
import Language.FCL.Error as Error
import Language.FCL.Storage as Storage
import Language.FCL.Metadata as Metadata
import Language.FCL.Encoding as Encoding
import Language.FCL.Analysis as Analysis
import Language.FCL.Undefinedness as Undefinedness
import Language.FCL.ReachabilityGraph as Reachability
import qualified Language.FCL.Duplicate as Dupl
import Numeric.Lossless.Decimal
import Numeric.Lossless.Number
import qualified Language.FCL.LanguageServerProtocol as LSP

-------------------------
-- Helper functions
-------------------------

simpleStringSchema :: Text -> NamedSchema
simpleStringSchema name = NamedSchema (Just name)
  $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }

objectNamedSchema :: Text -> [(Text, Referenced Schema)] -> Bool -> NamedSchema
objectNamedSchema name props required
  = NamedSchema (Just name) $ objectSchema props required

objectSchema :: [(Text, Referenced Schema)] -> Bool -> Schema
objectSchema props required = mempty
  { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
  , _schemaProperties = fromList props
  , _schemaRequired = if required then fst <$> props else []
  }

arrayNamedSchema :: Text -> Maybe [Referenced Schema] -> NamedSchema
arrayNamedSchema name = NamedSchema (Just name) . arraySchema


arraySchema :: Maybe [Referenced Schema] -> Schema
arraySchema elemsM = mempty
  { _schemaParamSchema = mempty { _paramSchemaType = SwaggerArray
                                , _paramSchemaItems = SwaggerItemsArray <$> elemsM
                                }
  }

nullSchema :: Schema
nullSchema = mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerNull }}

------------------------------
-- Orphan ToSchema instances
------------------------------

instance ToSchema Key.Signature where
  declareNamedSchema _ = pure $ simpleStringSchema "Signature"

instance ToSchema EvalFail where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema AST.Value where
  declareNamedSchema _ = do
    n <- declareSchemaRef @Number Proxy
    b <- declareSchemaRef @Bool Proxy
    aa <- declareSchemaRef @(Address AAccount) Proxy
    as <- declareSchemaRef @(Address AAsset) Proxy
    ac <- declareSchemaRef @(Address AContract) Proxy
    t <- declareSchemaRef @Text Proxy
    ii <- declareSchemaRef @(Integer, Integer) Proxy
    d <- declareSchemaRef @DateTime Proxy
    tm <- declareSchemaRef @TimeDelta Proxy
    ws <- declareSchemaRef @WorkflowState Proxy
    mvv <- declareSchemaRef @(Map Value Value) Proxy
    sv <- declareSchemaRef @(Set Value) Proxy
    nu <- declareSchemaRef @NameUpper Proxy
    vs <- declareSchemaRef @[Value] Proxy
    empt <- declareSchemaRef @() Proxy
    pure $ objectNamedSchema
      "Value"
      [("VNum", n)
      ,("VBool", b)
      ,("VAccount", aa)
      ,("VAsset", as)
      ,("VContract", ac)
      ,("VText", t)
      ,("VSig", ii)
      ,("VVoid", empt)
      ,("VDateTime", d)
      ,("VTimeDelta", tm)
      ,("VState", ws)
      ,("VMap", mvv)
      ,("VSet", sv)
      ,("VUndefined", empt)
      ,("VConstr", Inline $ arraySchema (Just [nu, vs]))
      ]
      False

instance ToSchema Contract where
  declareNamedSchema _ = do
    time <- declareSchemaRef @Timestamp Proxy
    t <- declareSchemaRef @Text Proxy
    gs <- declareSchemaRef @Storage Proxy
    m <- declareSchemaRef @[Name] Proxy
    w <- declareSchemaRef @WorkflowState Proxy
    aa <- declareSchemaRef @(Address AAccount) Proxy
    ac <- declareSchemaRef @(Address AContract) Proxy
    pure $ objectNamedSchema
      "Contract"
      [ ("timestamp", time)
      , ("script", t)
      , ("storage", gs)
      , ("state", w)
      , ("methods", m)
      , ("owner", aa)
      , ("address", ac)
      ]
      True

instance ToSchema InvalidMethodName
instance ToSchema GlobalStorage where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "GlobalStorage") (toSchema (Proxy :: Proxy Storage))

instance ToSchema Key where
  declareNamedSchema _ = pure $ simpleStringSchema "Key"
instance ToSchema Metadata where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "Metadata") (toSchema (Proxy :: Proxy (Map Text Text)))

instance ToSchema CallableMethods where
  declareNamedSchema _ = do
    m <- declareSchemaRef @(PermittedCallers, [(Name, Type)]) Proxy
    pure $ NamedSchema (Just "CallableMethods")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaAdditionalProperties = Just $ AdditionalPropertiesSchema m
               }

instance ToSchema PermittedCallers where
  declareNamedSchema _ = do
    t <- declareSchemaRef @Text Proxy
    pure $ NamedSchema (Just "PermittedCallers")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerArray }
               , _schemaAllOf = Just [t]
               }

instance ToSchema Def
instance ToSchema Type where
  declareNamedSchema _ = pure $ simpleStringSchema "Type"
instance ToSchema NumPrecision where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema TCollection where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Preconditions where
  declareNamedSchema _ = do
    tup <- declareSchemaRef @[(Precondition, LExpr)] Proxy
    pure $ NamedSchema (Just "Preconditions")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerArray }
               , _schemaAllOf = Just [tup]
               }

instance ToSchema Precondition where
  declareNamedSchema _ = pure $ simpleStringSchema "Precondition"

instance ToSchema (Located Expr) where
  declareNamedSchema _ = do
    l <- declareSchemaRef @Loc Proxy
    e <- declareSchemaRef @Expr Proxy
    pure $ objectNamedSchema "Located Expr" [("located", l), ("locVal", e)] True

instance ToSchema (Located Lit) where
  declareNamedSchema _ = do
    loc <- declareSchemaRef @Loc Proxy
    lit <- declareSchemaRef @Lit Proxy
    pure $ objectNamedSchema "Located Lit" [("located", loc), ("locVal", lit)] True

instance ToSchema (Located Name) where
  declareNamedSchema _ = do
    loc <- declareSchemaRef @Loc Proxy
    name <- declareSchemaRef @Name Proxy
    pure $ objectNamedSchema "Located Name" [("located", loc), ("locVal", name)] True

instance ToSchema (Located AST.Pattern) where
  declareNamedSchema _ = do
    loc <- declareSchemaRef @Loc Proxy
    pat <- declareSchemaRef @AST.Pattern Proxy
    pure $ objectNamedSchema "Located Pattern" [("located", loc), ("locVal", pat)] True

instance ToSchema (Located NameUpper) where
  declareNamedSchema _ = do
    loc <- declareSchemaRef @Loc Proxy
    nup <- declareSchemaRef @NameUpper Proxy
    pure $ objectNamedSchema "Located NameUpper" [("located", loc), ("locVal", nup)] True

instance ToSchema (Located BinOp) where
  declareNamedSchema _ = do
    loc <- declareSchemaRef @Loc Proxy
    bin <- declareSchemaRef @BinOp Proxy
    pure $ objectNamedSchema "Located BinOp" [("located", loc), ("locVal", bin)] True

instance ToSchema (Located UnOp) where
  declareNamedSchema _ = do
    loc <- declareSchemaRef (Proxy :: Proxy Loc)
    unp <- declareSchemaRef (Proxy :: Proxy UnOp)
    pure $ objectNamedSchema "Located UnOp" [("located", loc), ("locVal", unp)] True

instance ToSchema Loc where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Expr where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema BinOp
instance ToSchema UnOp
instance ToSchema CaseBranch
instance ToSchema AST.Pattern where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema PrimOp where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema AssetPrimOp
instance ToSchema MapPrimOp
instance ToSchema SetPrimOp
instance ToSchema CollPrimOp
instance ToSchema ADTDef
instance ToSchema ADTConstr
instance ToSchema Name where
  declareNamedSchema _ = pure $ simpleStringSchema "Name"

instance ToSchema TVar

instance ToSchema Lit
instance ToSchema Script
instance ToSchema Helper
instance ToSchema Transition
instance ToSchema WorkflowState where
  declareNamedSchema _ = pure $ simpleStringSchema "WorkflowState"

instance ToSchema Place where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Method
instance ToSchema Arg

instance ToSchema Decimal
instance ToSchema DateTime where
  declareNamedSchema _ = pure $ simpleStringSchema "DateTime"

instance ToSchema TimeDelta
instance ToSchema NameUpper
instance ToSchema Datetime where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema Datetime.Types.Delta
instance ToSchema Period where
  declareNamedSchema _ = do
    i <- declareSchemaRef (Proxy :: Proxy Int)
    pure $ objectNamedSchema "Period" [("periodDays", i), ("periodYears", i), ("periodMonths", i)] True

instance ToSchema Duration where
  declareNamedSchema _ = do
    i <- declareSchemaRef (Proxy :: Proxy Int)
    pure $ objectNamedSchema
      "Duration"
      [("durationHours", i), ("durationNs", i), ("durationSeconds", i), ("durationMinutes", i)]
      True

instance ToSchema (Address a) where
  declareNamedSchema proxy =
    pure $ NamedSchema Nothing $ mempty

instance ToSchema LSP.ReqDef  where
  declareNamedSchema _ = do
    t <- declareSchemaRef @Text Proxy
    pure $ objectNamedSchema "ReqDef" [("defType", t), ("defName", t), ("defValue", t)] True
    -- pure $ NamedSchema (Just "ReqDef")
    --   $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
    --            , _schemaProperties = fromList [("defType", t), ("defName", t), ("defValue", t)]
    --            , _schemaRequired = [ "defType", "defName" ]
    --            }

instance ToSchema LSP.ReqScript
instance ToSchema LSP.ReqADTDef
instance ToSchema LSP.ReqTransition
instance ToSchema LSP.ReqMethod
instance ToSchema LSP.ReqMethodArg where
  declareNamedSchema _ = do
    t <- declareSchemaRef @Text Proxy
    pure $ objectNamedSchema "ReqMethodArg" [("argType", t), ("argName", t)] True

instance ToSchema LSP.RespScript
instance ToSchema LSP.RespMethod
instance ToSchema LSP.RespDef
instance ToSchema Parser.ParseErrInfo
instance ToSchema Dupl.VarSrc where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Dupl.DuplicateError
instance ToSchema Typecheck.TypeError
instance ToSchema Typecheck.TypeErrInfo where
  declareNamedSchema proxy =
    pure $ NamedSchema Nothing $ mempty
instance ToSchema Analysis.TransitionErrors
instance ToSchema Analysis.TransitionError
instance ToSchema Reachability.WFError
instance ToSchema Undefinedness.InvalidStackTrace where
  declareNamedSchema proxy =
    pure $ NamedSchema Nothing $ mempty
instance ToSchema Effect.EffectError
instance ToSchema Compile.CompilationErr
instance ToSchema LSP.LSPErr
instance ToSchema LSP.LSP
instance ToSchema Sig
instance ToSchema Effects
instance ToSchema Effect
instance ToSchema Warning


instance ToSchema InvalidSignature where
  declareNamedSchema _ = do
    t <- declareSchemaRef @Text Proxy
    s <- declareSchemaRef @Key.Signature Proxy
    pure $ objectNamedSchema
      "InvalidSignature"
      [("InvalidSignature", Inline $ arraySchema (Just [s, t]))
      ,("DecodeSignatureFail", t)
      ,("SignatureSplittingFail", t)
      ]
      False

instance ToSchema Balance where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "Balance") (toSchema (Proxy :: Proxy Decimal))

instance ToSchema Number
instance ToSchema (Ratio Integer) where
  declareNamedSchema _ = do
    i <- declareSchemaRef @Integer Proxy
    pure $ objectNamedSchema "Ratio Integer" [("numerator", i), ("denominator", i)] True

instance ToSchema (Hash Encoding.Base16ByteString) where
  declareNamedSchema _ = pure $ simpleStringSchema "Hash Base16ByteString"

instance ToSchema Encoding.Base16ByteString where
  declareNamedSchema _ = pure $ simpleStringSchema "Base16ByteString"

instance ToSchema Encoding.Base64ByteString where
  declareNamedSchema _ = pure $ simpleStringSchema "Base64ByteString"

instance ToSchema Encoding.Base64PByteString where
  declareNamedSchema _ = pure $ simpleStringSchema "Base64PByteString"

instance ToSchema Encoding.Base58ByteString where
  declareNamedSchema _ = pure $ simpleStringSchema "Base58ByteString"
