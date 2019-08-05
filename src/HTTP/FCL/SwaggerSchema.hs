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
import Data.Swagger.Declare
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

instance ToSchema Key.Signature where
  declareNamedSchema _ = simpleStringSchema "Signature"

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
    pure . NamedSchema (Just "Value") $
      mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
             , _schemaProperties =
                 fromList [("VNum", n)
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
                          ,("VConstr", Inline $ mempty
                                        { _schemaParamSchema = mempty { _paramSchemaType = SwaggerArray
                                                                      , _paramSchemaItems = Just $ SwaggerItemsArray [nu, vs]
                                                                      }
                                        })
                          ]
             }

instance ToSchema Contract where
  declareNamedSchema _ = do
    time <- declareSchemaRef @Timestamp Proxy
    t <- declareSchemaRef @Text Proxy
    gs <- declareSchemaRef @Storage Proxy
    m <- declareSchemaRef @[Name] Proxy
    w <- declareSchemaRef @WorkflowState Proxy
    aa <- declareSchemaRef @(Address AAccount) Proxy
    ac <- declareSchemaRef @(Address AContract) Proxy
    pure . NamedSchema (Just "Contract")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [ ("timestamp", time)
                                              , ("script", t)
                                              , ("storage", gs)
                                              , ("state", w)
                                              , ("methods", m)
                                              , ("owner", aa)
                                              , ("address", ac)
                                              ]
               , _schemaRequired = [ "timestamp", "script", "storage", "methods", "owner", "address"  ]
               }

instance ToSchema InvalidMethodName
instance ToSchema GlobalStorage where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "GlobalStorage") (toSchema (Proxy :: Proxy Storage))

instance ToSchema Key where
  declareNamedSchema _ = simpleStringSchema "Key"
instance ToSchema Metadata where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "Metadata") (toSchema (Proxy :: Proxy (Map Text Text)))

instance ToSchema CallableMethods where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "CallableMethods")
    $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerArray } }
    -- TODO: Find out error: "Unknown schema" when the following code is uncommented.
    -- (toSchema (Proxy :: Proxy (Map Name (PermittedCallers, [(Name, Type)]))))

instance ToSchema PermittedCallers where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema Def
instance ToSchema Type where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema NumPrecision where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema TCollection where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Preconditions where
  declareNamedSchema _ = do
    tup <- declareSchemaRef (Proxy :: Proxy [(Precondition, LExpr)])
    pure $ NamedSchema (Just "Preconditions")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerArray }
               , _schemaAllOf = Just [tup]
               }

instance ToSchema Precondition where
  declareNamedSchema _ = do
    pure . NamedSchema (Just "Precondition")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }

instance ToSchema (Located Expr) where
  declareNamedSchema _ = do
    l <- declareSchemaRef (Proxy :: Proxy Loc)
    e <- declareSchemaRef (Proxy :: Proxy Expr)
    pure . NamedSchema (Just "Located Expr")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", e)]
               , _schemaRequired = [ "located", "locVal"  ]
               }

instance ToSchema (Located Lit) where
  declareNamedSchema _ = do
    loc <- declareSchemaRef (Proxy :: Proxy Loc)
    lit <- declareSchemaRef (Proxy :: Proxy Lit)
    pure . NamedSchema (Just "Located Lit")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", loc), ("locVal", lit)]
               , _schemaRequired = [ "located", "locVal"  ]
               }

instance ToSchema (Located Name) where
  declareNamedSchema _ = do
    l <- declareSchemaRef (Proxy :: Proxy Loc)
    t <- declareSchemaRef (Proxy :: Proxy Name)
    pure $ NamedSchema (Just "Located Name")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", t)]
               , _schemaRequired = [ "located", "locVal" ]
               }


instance ToSchema (Located AST.Pattern) where
  declareNamedSchema _ = do
    l <- declareSchemaRef (Proxy :: Proxy Loc)
    p <- declareSchemaRef (Proxy :: Proxy AST.Pattern)
    pure $ NamedSchema (Just "Located")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", p)]
               , _schemaRequired = [ "located", "locVal"  ]
               }
instance ToSchema (Located NameUpper) where
  declareNamedSchema _ = do
    l <- declareSchemaRef (Proxy :: Proxy Loc)
    n <- declareSchemaRef (Proxy :: Proxy NameUpper)
    pure $ NamedSchema (Just "Located NameUpper")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", n)]
               , _schemaRequired = [ "located", "locVal"  ]
               }
instance ToSchema (Located BinOp) where
  declareNamedSchema _ = do
    l <- declareSchemaRef (Proxy :: Proxy Loc)
    t <- declareSchemaRef (Proxy :: Proxy BinOp)
    pure $ NamedSchema (Just "Located BinOp")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", t)]
               , _schemaRequired = [ "located", "locVal"  ]
               }

instance ToSchema (Located UnOp) where
  declareNamedSchema _ = do
    l <- declareSchemaRef (Proxy :: Proxy Loc)
    u <- declareSchemaRef (Proxy :: Proxy UnOp)
    pure $ NamedSchema (Just "Located UnOp")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", u)]
               , _schemaRequired = [ "located", "locVal"  ]
               }

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
  declareNamedSchema _ = simpleStringSchema "Name"

instance ToSchema TVar

instance ToSchema Lit
instance ToSchema Script
instance ToSchema Helper
instance ToSchema Transition
instance ToSchema WorkflowState where
  declareNamedSchema _ = simpleStringSchema "WorkflowState"

instance ToSchema Place where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Method
instance ToSchema Arg

instance ToSchema Decimal
instance ToSchema DateTime where
  declareNamedSchema _ = simpleStringSchema "DateTime"

instance ToSchema TimeDelta
instance ToSchema NameUpper
instance ToSchema Datetime where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema Datetime.Types.Delta
instance ToSchema Period where
  declareNamedSchema _ = do
    i <- declareSchemaRef (Proxy :: Proxy Int)
    pure $ NamedSchema (Just "Period")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("periodDays", i), ("periodYears", i), ("periodMonths", i)]
               , _schemaRequired = [ "periodDays", "periodYears", "periodMonths" ]
               }
instance ToSchema Duration where
  declareNamedSchema _ = do
    i <- declareSchemaRef (Proxy :: Proxy Int)
    pure $ NamedSchema (Just "Duration")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("durationHours", i), ("durationNs", i), ("durationSeconds", i), ("durationMinutes", i)]
               , _schemaRequired = [ "durationHours", "durationNs", "durationSeconds", "durationMinutes" ]
               }
instance ToSchema (Address a) where
  declareNamedSchema proxy =
    pure $ NamedSchema Nothing $ mempty

instance ToSchema LSP.ReqDef  where
  declareNamedSchema _ = do
    t <- declareSchemaRef (Proxy :: Proxy Text)
    pure $ NamedSchema (Just "ReqDef")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("defType", t), ("defName", t), ("defValue", t)]
               , _schemaRequired = [ "defType", "defName" ]
               }

instance ToSchema LSP.ReqScript
instance ToSchema LSP.ReqADTDef
instance ToSchema LSP.ReqTransition
instance ToSchema LSP.ReqMethod
instance ToSchema LSP.ReqMethodArg where
  declareNamedSchema _ = do
    t <- declareSchemaRef (Proxy :: Proxy Text)
    pure $ NamedSchema (Just "ReqMethodArg")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("argType", t), ("argName", t)]
               , _schemaRequired = [ "argType", "argName" ]
               }

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
    pure . NamedSchema (Just "Ratio") $
      mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
             , _schemaProperties = fromList [("InvalidSignature", Inline $ mempty
                                               { _schemaParamSchema = mempty { _paramSchemaType = SwaggerArray
                                                                             , _paramSchemaItems = Just $ SwaggerItemsArray [s, t]
                                                                             }
                                               })
                                            ,("DecodeSignatureFail", t)
                                            ,("SignatureSplittingFail", t)
                                            ]
             }
instance ToSchema Balance where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "Balance") (toSchema (Proxy :: Proxy Decimal))

instance ToSchema Number
instance ToSchema (Ratio Integer) where
  declareNamedSchema _ = do
    i <- declareSchemaRef (Proxy :: Proxy Integer)
    pure . NamedSchema (Just "Ratio") $
      mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
             , _schemaProperties = fromList [("numerator", i), ("denominator", i)]
             , _schemaRequired = [ "numerator", "denominator" ]
             }

instance ToSchema (Hash Encoding.Base16ByteString) where
  declareNamedSchema _ = simpleStringSchema "Hash Base16ByteString"

instance ToSchema Encoding.Base16ByteString where
  declareNamedSchema _ = simpleStringSchema "Base16ByteString"

instance ToSchema Encoding.Base64ByteString where
  declareNamedSchema _ = simpleStringSchema "Base64ByteString"

instance ToSchema Encoding.Base64PByteString where
  declareNamedSchema _ = simpleStringSchema "Base64PByteString"

instance ToSchema Encoding.Base58ByteString where
  declareNamedSchema _ = simpleStringSchema "Base58ByteString"

simpleStringSchema :: Text -> Declare (Definitions Schema) NamedSchema
simpleStringSchema name = do
    pure $ NamedSchema (Just name)
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }
