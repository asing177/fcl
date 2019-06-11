{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Protolude

import Web.Scotty as WS
import qualified Data.Text.Lazy as TL

import Language.FCL.AST as Script
import qualified Language.FCL.Utils as Utils
import qualified Language.FCL.Compile as Compile
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Graphviz as Graphviz
import qualified Language.FCL.Typecheck as Typecheck
import qualified Language.FCL.Parser as Parser
import qualified Language.FCL.LanguageServerProtocol as LSP
import Data.Aeson as A (ToJSON(..), FromJSON(..), Value, (.=), (.:), object)

data RPCResponseError
  = LSPErr [LSP.LSP]
  | ContractCompile Compile.CompilationErr
  | ContractParse Parser.ParseErrInfo
  | TypeErr Text
  deriving (Generic)

instance ToJSON RPCResponseError where
  toJSON (LSPErr msg) = toJSON msg
  toJSON (ContractParse parseErr) = object
    [ "errorType"      .= ("ContractParse" :: Text)
    , "errorMsg"       .= Parser.errMsg parseErr
    , "errorPosition"  .= object [
          "line"   .= Parser.line parseErr
        , "column" .= Parser.column parseErr
        ]
    ]
  toJSON (ContractCompile script) = toJSON script

  toJSON (TypeErr msg) = object
    [ "errorType" .= ("JSONParse" :: Text)
    , "errorMsg"  .= msg
    ]

-- | An RPC response body
data RPCResponse
  = RPCResp { contents :: A.Value }
  | RPCRespError RPCResponseError
  | RPCRespOK
  | RPCTransactionOK { txHash :: Text }
  deriving (Generic, ToJSON)

rpcApi :: ScottyM ()
rpcApi = do
    -- | Compile an FCL definition given in plain text
    -- E.g. "text x = "foo";"
    -- It returns a JSON encoding of a Def type
    post "/defs/compile/raw" $ do
      def <- WS.body
      case Parser.parseDefn (toS def) of
        Left err -> jsonContractParseErr err
        Right defn
          -> case Typecheck.runSolverM . snd
              $ Typecheck.runInferM
                  (Script.ADTInfo mempty mempty)
                  Typecheck.emptyInferState
                  (Typecheck.tcDefn defn) of
          Left err ->  WS.json . RPCRespError . TypeErr
            $ show (Pretty.ppr err)
          Right _ -> jsonRPCRespM defn

    -- | Compile an FCL script given in plain text
    -- If successful, it returns the compiled script along with
    -- warnings, the given script and its representation in graphviz format
    post "/scripts/compile/raw" $ do
      body <- WS.body
      case Compile.compile (toS body) of
        Left err -> do
          print (Pretty.prettyPrint err)
          jsonLSPErr $ LSP.toLSP err
        Right cs -> do
          let script = Compile.checkedScript cs
          jsonRPCRespM
            $ LSP.RespScript
              { respScript = script
              , respPpScript = toS $ Pretty.prettyPrint script
              , respScriptWarnings = Compile.checkedScriptWarnings cs
              , respScriptSigs = Compile.checkedScriptSigs cs
              , respGraphviz = Graphviz.methodsToGraphviz (Script.scriptMethods script)
              }

    -- | Validate a script given a JSON encoding of a ReqScript type
    -- It returns RespScript
    post "/scripts/compile" $ do
      body <- WS.jsonData
      case LSP.validateScript body of
        Left err -> jsonCompilationErr err
        Right resp -> jsonRPCRespM resp

    -- | Parse a method given a JSON encoding of a ReqMethod type
    -- It returns RespMethod
    post "/methods/parse" $ do
      body <- WS.jsonData
      case LSP.parseMethod body of
        Left err -> jsonContractParseErr err
        Right method -> jsonRPCRespM
          $ LSP.RespMethod method (toS $ Pretty.prettyPrint method)

    -- | Parse a script given a JSON encoding of a ReqScript type
    -- It returns RespScript
    post "/scripts/parse" $ do
      body <- WS.jsonData
      case LSP.parseScript body of
        Left err -> jsonContractParseErr err
        Right script -> jsonRPCRespM
          $ LSP.RespScript
            script
            (Pretty.prettyPrint script)
            []
            []
            (Graphviz.methodsToGraphviz (Script.scriptMethods script))
    where
      jsonLSPErr :: [LSP.LSP] -> WS.ActionM ()
      jsonLSPErr = WS.json . RPCRespError . LSPErr

      jsonRPCRespM :: ToJSON a => a -> WS.ActionM ()
      jsonRPCRespM = WS.json . RPCResp . toJSON

      jsonContractParseErr :: Parser.ParseErrInfo -> WS.ActionM ()
      jsonContractParseErr = WS.json . RPCRespError . ContractParse

      jsonCompilationErr :: Compile.CompilationErr -> WS.ActionM ()
      jsonCompilationErr = WS.json . RPCRespError . ContractCompile

main :: IO ()
main = do
  scotty 8001 rpcApi
