{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances          #-}
module HTTP.FCL.SwaggerSchema () where

import Protolude hiding (get, from, Type)
import Data.Swagger
import Datetime.Types
import Data.HashMap.Strict.InsOrd

import Language.FCL.AST as AST hiding (at)
import Language.FCL.Prim
import Language.FCL.Address
import Language.FCL.Parser as Parser
import Language.FCL.Typecheck as Typecheck
import Language.FCL.Compile as Compile
import Language.FCL.Warning as Warning
import Language.FCL.Effect as Effect
import Language.FCL.Encoding as Encoding
import Language.FCL.Analysis as Analysis
import Language.FCL.Undefinedness as Undefinedness
import Language.FCL.Reachability.Definitions as Reachability
import qualified Language.FCL.Duplicate as Dupl
import Numeric.Lossless.Decimal
import qualified Language.FCL.LanguageServerProtocol as LSP

instance ToSchema Def
instance ToSchema Type where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema NumPrecision where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema TCollection where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Preconditions where
  declareNamedSchema _ = do
    x <- declareSchemaRef (Proxy :: Proxy (Precondition, LExpr))
    pure $ NamedSchema (Just "Preconditions")
      $ mempty { _schemaParamSchema =
                   mempty { _paramSchemaType = SwaggerArray
                          -- , _paramSchemaItems = Just $ SwaggerItemsArray [x]
                          }
               }

instance ToSchema Precondition where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Precondition")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }

instance ToSchema (Located Expr) where
  declareNamedSchema _ = do
    l <- declareSchemaRef (Proxy :: Proxy Loc)
    t <- declareSchemaRef (Proxy :: Proxy Expr)
    pure $ NamedSchema (Just "Located Expr")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", t)]
               , _schemaRequired = [ "located", "locVal"  ]
               }

instance ToSchema (Located Lit) where
  declareNamedSchema _ = do
    l <- declareSchemaRef (Proxy :: Proxy Loc)
    t <- declareSchemaRef (Proxy :: Proxy Lit)
    pure $ NamedSchema (Just "Located Lit")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", t)]
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
    t <- declareSchemaRef (Proxy :: Proxy AST.Pattern)
    pure $ NamedSchema (Just "Located")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", t)]
               , _schemaRequired = [ "located", "locVal"  ]
               }
instance ToSchema (Located NameUpper) where
  declareNamedSchema _ = do
    l <- declareSchemaRef (Proxy :: Proxy Loc)
    t <- declareSchemaRef (Proxy :: Proxy NameUpper)
    pure $ NamedSchema (Just "Located NameUpper")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", t)]
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
    t <- declareSchemaRef (Proxy :: Proxy UnOp)
    pure $ NamedSchema (Just "Located UnOp")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("located", l), ("locVal", t)]
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
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "Name")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }

instance ToSchema TVar

instance ToSchema Lit
instance ToSchema Script
instance ToSchema Helper
instance ToSchema Transition
instance ToSchema WorkflowState where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "WorkflowState")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }
instance ToSchema Place where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Method
instance ToSchema Arg

instance ToSchema Decimal
instance ToSchema DateTime where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "DateTime")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }
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

instance ToSchema Encoding.Base16ByteString where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "Base16ByteString")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }

instance ToSchema Encoding.Base64ByteString where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "Base64ByteString")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }
instance ToSchema Encoding.Base64PByteString where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "Base64PByteString")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }
instance ToSchema Encoding.Base58ByteString where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "Base58ByteString")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }
