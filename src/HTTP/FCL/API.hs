{-# options_ghc -fno-warn-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module HTTP.FCL.API
  ( FCLAPI
  , fclProxyAPI
  , fclServer
  , runHttpFcl
  ) where

import Protolude hiding (get, from, Type)
import Network.Wai.Handler.Warp

import Servant.Server
import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Servant.Checked.Exceptions
import Network.Wai.Middleware.Cors

import qualified Language.FCL.LanguageServerProtocol as LSP
import Language.FCL.LanguageServerProtocol (LSPErr)

import HTTP.FCL.SwaggerSchema()

--------------------
-- Scripts
--------------------

type ScriptCompile = ReqBody '[JSON] LSP.ReqScript :> Throws LSPErr :> Post '[JSON] LSP.RespScript
type ScriptCompileRaw = "raw" :> Throws LSPErr :> ReqBody '[PlainText] Text :> Post '[JSON] LSP.RespScript
type ScriptParse = Throws LSPErr :> ReqBody '[JSON] LSP.ReqScript :> Post '[JSON] LSP.RespScript

type ScriptsAPI = "scripts" :>
 ( ("compile" :> (ScriptCompile :<|> ScriptCompileRaw)) :<|>
   ("parse" :> ScriptParse)
 )

scriptsCompile :: LSP.ReqScript -> Handler (Envelope '[LSPErr] LSP.RespScript)
scriptsCompile req
  = either pureErrEnvelope pureSuccEnvelope (LSP.scriptCompile req)

scriptsCompileRaw :: Text -> Handler (Envelope '[LSPErr] LSP.RespScript)
scriptsCompileRaw req
  = either pureErrEnvelope pureSuccEnvelope (LSP.scriptCompileRaw req)

scriptsParse :: LSP.ReqScript -> Handler (Envelope '[LSPErr] LSP.RespScript)
scriptsParse req
  = either pureErrEnvelope pureSuccEnvelope (LSP.scriptParse req)

--------------------
-- Methods
--------------------

type MethodsCompileRaw = "raw" :> Throws LSPErr :> ReqBody '[PlainText] Text :> Post '[JSON] (LSP.RespMethod)
type MethodsCompile = Throws LSPErr :> ReqBody '[JSON] LSP.ReqMethod :> Post '[JSON] (LSP.RespMethod)

type MethodsAPI = "methods" :>
 ("compile" :> (MethodsCompile :<|> MethodsCompileRaw))

methodCompileRaw :: Text -> Handler (Envelope '[LSPErr] LSP.RespMethod)
methodCompileRaw req
  = either pureErrEnvelope pureSuccEnvelope (LSP.methodCompileRaw req)

methodCompile :: LSP.ReqMethod -> Handler (Envelope '[LSPErr] LSP.RespMethod)
methodCompile req
  = either pureErrEnvelope pureSuccEnvelope (LSP.methodCompile req)

--------------------
-- Defs
--------------------

type DefsCompileRaw = "raw" :> Throws LSPErr :> ReqBody '[PlainText] Text :> Post '[JSON] (LSP.RespDef)
type DefsCompile = Throws LSPErr :> ReqBody '[JSON] LSP.ReqDef :> Post '[JSON] (LSP.RespDef)

type DefsAPI = "defs" :>
  ("compile" :> (DefsCompile :<|> DefsCompileRaw))

defsCompileRaw :: Text -> Handler (Envelope '[LSPErr] LSP.RespDef)
defsCompileRaw req
  = either pureErrEnvelope pureSuccEnvelope (LSP.defCompileRaw req)

defsCompile :: LSP.ReqDef -> Handler (Envelope '[LSPErr] LSP.RespDef)
defsCompile req
    = either pureErrEnvelope pureSuccEnvelope (LSP.defCompile req)

----------------------
-- Server
----------------------

instance ErrStatus LSPErr where
  toErrStatus _ = toEnum 400

instance (ErrStatus err, HasSwagger sub) => HasSwagger (Throws err :> sub)
  where
    toSwagger _ =
      toSwagger (Proxy :: Proxy sub) &
        setResponse
          (fromEnum $ toErrStatus (witness :: err))
          (pure mempty)

type FCLAPI = ScriptsAPI :<|> MethodsAPI :<|> DefsAPI

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> FCLAPI

fclProxyAPI :: Proxy FCLAPI
fclProxyAPI = Proxy

api :: Proxy API
api = Proxy

fclServer :: Server FCLAPI
fclServer = (scripts :<|> methods :<|> defs)
  where
    scripts = (scriptsCompile :<|> scriptsCompileRaw) :<|> scriptsParse
    methods =  methodCompile :<|> methodCompileRaw
    defs = defsCompile :<|> defsCompileRaw

appServer :: Server API
appServer =
  swaggerSchemaUIServer (toSwagger fclProxyAPI) :<|> fclServer

runHttpFcl :: IO ()
runHttpFcl = do
  let port = 8080
  putText $ "Running on port " <> show port
  run port . customCors . serve api $ appServer
  where
    customCors = cors (const $ Just (simpleCorsResourcePolicy  { corsRequestHeaders = ["Accept", "Accept-Language", "Content-Language", "Content-Type"] }))
