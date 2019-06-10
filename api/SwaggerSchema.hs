{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances          #-}
module SwaggerSchema where

import Protolude hiding (get, from, Type)
import Data.Swagger

import Datetime.Types

import Language.FCL.AST as Script
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
instance ToSchema Preconditions
instance ToSchema Precondition
instance ToSchema a => ToSchema (Located a)
instance ToSchema Loc where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Expr where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema BinOp
instance ToSchema UnOp
instance ToSchema Match
instance ToSchema Script.Pattern
instance ToSchema PrimOp where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema AssetPrimOp
instance ToSchema MapPrimOp
instance ToSchema SetPrimOp
instance ToSchema CollPrimOp
instance ToSchema EnumDef
instance ToSchema EnumConstr
instance ToSchema Name
instance ToSchema TVar
instance ToSchema Lit where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Script
instance ToSchema Helper
instance ToSchema Transition
instance ToSchema WorkflowState
instance ToSchema Place where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Method
instance ToSchema Arg
instance ToSchema SafeString where
  declareNamedSchema proxy =
    return $ NamedSchema Nothing $ mempty

instance ToSchema Decimal
instance ToSchema SafeInteger
instance ToSchema DateTime
instance ToSchema TimeDelta
instance ToSchema Datetime where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema Datetime.Types.Delta
instance ToSchema Period where
  declareNamedSchema _ =
    return $ NamedSchema Nothing $ mempty

instance ToSchema Duration where
  declareNamedSchema _ =
    return $ NamedSchema Nothing $ mempty
instance ToSchema (Address a) where
  declareNamedSchema proxy =
    return $ NamedSchema Nothing $ mempty

instance ToSchema LSP.ReqDef
instance ToSchema LSP.ReqScript
instance ToSchema LSP.ReqEnumDef
instance ToSchema LSP.ReqTransition
instance ToSchema LSP.ReqMethod
instance ToSchema LSP.ReqMethodArg
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
    return $ NamedSchema Nothing $ mempty
instance ToSchema Analysis.TransitionErrors
instance ToSchema Analysis.TransitionError
instance ToSchema Reachability.WFError
instance ToSchema Undefinedness.InvalidStackTrace where
  declareNamedSchema proxy =
    return $ NamedSchema Nothing $ mempty
instance ToSchema Effect.EffectError
instance ToSchema Compile.CompilationErr
instance ToSchema LSP.LSPErr
instance ToSchema LSP.LSP
instance ToSchema Sig
instance ToSchema Effects
instance ToSchema Effect
instance ToSchema Warning
