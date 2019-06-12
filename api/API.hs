{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances          #-}
module API where

import Network.Wai.Handler.Warp
import Protolude hiding (get, from, Type)
import Servant.Server
import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Network.Wai.Middleware.Cors

import Language.FCL.Parser as Parser
import Language.FCL.Pretty as Pretty
import Language.FCL.Compile as Compile
import qualified Language.FCL.LanguageServerProtocol as LSP
import Data.Aeson as A (ToJSON(..), (.=), object)

import SwaggerSchema()

data Config = Config

newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServantErr m) a
  } deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

type App = AppT IO

type AppAPI = ScriptsAPI :<|> MethodsAPI :<|> DefsAPI

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> AppAPI

data RPCResponseError
  = RPCLSPErr LSP.LSPErr
  deriving (Show, Generic)

instance ToJSON RPCResponseError
instance ToSchema RPCResponseError

-- | An RPC response body
data RPCResponse a
  = RPCResp { contents :: a }
  | RPCRespError RPCResponseError
  | RPCRespOK
  deriving (Show, Generic)

instance ToJSON a => ToJSON (RPCResponse a)

instance (ToSchema a) => ToSchema (RPCResponse a) where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

--------------------
-- Scripts
--------------------

type ScriptCompile = ReqBody '[JSON] LSP.ReqScript :> Post '[JSON] (RPCResponse LSP.RespScript)
type ScriptCompileRaw = "raw" :> ReqBody '[PlainText] Text :> Post '[JSON] (RPCResponse LSP.RespScript)
type ScriptParse = ReqBody '[JSON] LSP.ReqScript :> Post '[JSON] (RPCResponse LSP.RespScript)

type ScriptsAPI = "scripts" :>
 ( ("compile" :> (ScriptCompile :<|> ScriptCompileRaw)) :<|>
   ("parse" :> ScriptParse)
 )

scriptsCompile :: LSP.ReqScript -> App (RPCResponse LSP.RespScript)
scriptsCompile req
  = case LSP.scriptCompile req of
      Left err -> pure . RPCRespError . RPCLSPErr $ err
      Right resp -> pure  . RPCResp $ resp

scriptsCompileRaw :: Text -> App (RPCResponse LSP.RespScript)
scriptsCompileRaw body
  = case LSP.scriptCompileRaw body of
      Left err -> pure . RPCRespError . RPCLSPErr $ err
      Right script -> pure  . RPCResp $ script

scriptsParse :: LSP.ReqScript -> App (RPCResponse LSP.RespScript)
scriptsParse req
 = case LSP.scriptParse req of
     Left err -> pure $ RPCRespError . RPCLSPErr $ err
     Right script -> pure $ RPCResp script

--------------------
-- Methods
--------------------

type MethodsCompileRaw = "raw" :> ReqBody '[PlainText] Text :> Post '[JSON] (RPCResponse LSP.RespMethod)
type MethodsCompile = ReqBody '[JSON] LSP.ReqMethod :> Post '[JSON] (RPCResponse LSP.RespMethod)

type MethodsAPI = "methods" :>
 ("compile" :> (MethodsCompile :<|> MethodsCompileRaw))

methodCompileRaw :: Text -> App (RPCResponse LSP.RespMethod)
methodCompileRaw text
  = case LSP.methodCompileRaw text of
      Left err -> pure . RPCRespError . RPCLSPErr $ err
      Right method -> pure . RPCResp $ method

methodCompile :: LSP.ReqMethod -> App (RPCResponse LSP.RespMethod)
methodCompile req
  = case LSP.methodCompile req of
        Left err -> pure . RPCRespError . RPCLSPErr $ err
        Right method -> pure . RPCResp $ method

--------------------
-- Defs
--------------------

type DefsCompileRaw = "raw" :> ReqBody '[PlainText] Text :> Post '[JSON] (RPCResponse LSP.RespDef)
type DefsCompile = ReqBody '[JSON] LSP.ReqDef :> Post '[JSON] (RPCResponse LSP.RespDef)

type DefsAPI = "defs" :>
  ("compile" :> (DefsCompile :<|> DefsCompileRaw))

defsCompileRaw :: Text -> App (RPCResponse LSP.RespDef)
defsCompileRaw text = case LSP.defCompileRaw text of
  Left err -> pure . RPCRespError . RPCLSPErr $ err
  Right defn -> pure . RPCResp $ defn

defsCompile :: LSP.ReqDef -> App (RPCResponse LSP.RespDef)
defsCompile req = case LSP.defCompile req of
  Left err -> pure . RPCRespError . RPCLSPErr $ err
  Right defn -> pure . RPCResp $ defn

----------------------
-- Server
----------------------

appAPI :: Proxy AppAPI
appAPI = Proxy

api :: Proxy API
api = Proxy

server :: ServerT AppAPI App
server = scripts :<|> methods :<|> defs
  where
    scripts = (scriptsCompile :<|> scriptsCompileRaw) :<|> scriptsParse
    methods =  methodCompile :<|> methodCompileRaw
    defs = defsCompile :<|> defsCompileRaw

runAppAsHandler :: Config -> App a -> Handler a
runAppAsHandler cfg app = Handler $ runReaderT (runApp app) cfg

appToServer :: Config -> Server API
appToServer cfg =
  swaggerSchemaUIServer (toSwagger appAPI)
    :<|> (hoistServer appAPI (runAppAsHandler cfg) server)

runAPI :: IO ()
runAPI = do
  let port = 8080
  putText $ "Running on port " <> show port
  run port . simpleCors . serve api $ (appToServer Config)
