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
import Language.FCL.Unsafe
import Language.FCL.Address
import Language.FCL.Parser as Parser
import Language.FCL.Typecheck as Typecheck
import Language.FCL.Compile as Compile
import Language.FCL.Warning as Warning
import Language.FCL.Effect as Effect
import Language.FCL.Analysis as Analysis
import Language.FCL.Undefinedness as Undefinedness
import Language.FCL.ReachabilityGraph as Reachability
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
    p <- declareSchemaRef (Proxy :: Proxy Precondition)
    pure $ NamedSchema (Just "Preconditions")
      $ mempty { _schemaParamSchema =
                   mempty { _paramSchemaType = SwaggerString
                          , _paramSchemaItems = Just $ SwaggerItemsArray [p, p]
                          }
               }

instance ToSchema Precondition where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Precondition")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }

instance ToSchema a => ToSchema (Located a)
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
instance ToSchema ADTConstr where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "EnumConstr")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }

instance ToSchema Name where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "Name")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }

instance ToSchema TVar
instance ToSchema Lit where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
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
instance ToSchema SafeString where
  declareNamedSchema proxy =
    pure $ NamedSchema Nothing $ mempty

instance ToSchema Decimal
instance ToSchema SafeInteger
instance ToSchema DateTime
instance ToSchema TimeDelta
instance ToSchema NameUpper
instance ToSchema Datetime where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema Datetime.Types.Delta
instance ToSchema Period where
  declareNamedSchema _ =
    pure $ NamedSchema Nothing $ mempty

instance ToSchema Duration where
  declareNamedSchema _ =
    pure $ NamedSchema Nothing $ mempty
instance ToSchema (Address a) where
  declareNamedSchema proxy =
    pure $ NamedSchema Nothing $ mempty

instance ToSchema LSP.ReqDef  where
  declareNamedSchema _ = do
    t <- declareSchemaRef (Proxy :: Proxy Text)
    mt <- declareSchemaRef (Proxy :: Proxy (Maybe Text))
    pure $ NamedSchema (Just "ReqDef")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("defType", t), ("defName", t), ("defValue", mt)]
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
