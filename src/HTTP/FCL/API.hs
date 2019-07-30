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
  , runHttpFclAsHandler
  , runHttpFcl
  , RPCResponseError(..)
  , RPCResponse(..)
  ) where

import Protolude hiding (get, from, Type)
import Test.QuickCheck
import Network.Wai.Handler.Warp
import Data.HashMap.Strict.InsOrd

import Servant.Server
import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Network.Wai.Middleware.Cors

import qualified Language.FCL.LanguageServerProtocol as LSP
import Data.Aeson as A

import HTTP.FCL.SwaggerSchema()

data Config = Config

newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServantErr m) a
  } deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

type App = AppT IO

type FCLAPI resp = ScriptsAPI resp :<|> MethodsAPI resp :<|> DefsAPI resp

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> FCLAPI RPCResponse

data RPCResponseError
  = RPCLSPErr LSP.LSPErr
  deriving (Show, Generic)

instance ToJSON RPCResponseError
instance ToSchema RPCResponseError where
  declareNamedSchema _ = do
    t <- declareSchemaRef (Proxy :: Proxy Text)
    l <- declareSchemaRef (Proxy :: Proxy [LSP.LSP])
    pure $ NamedSchema (Just "RPCResponseError")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("lsp", l), ("errorMsg", t)]
               , _schemaRequired = [ "lsp", "errorMsg" ]
               }

-- | An RPC response body
data RPCResponse a
  = RPCResp a
  | RPCRespError RPCResponseError
  deriving (Show, Generic)

instance ToJSON a => ToJSON (RPCResponse a) where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToSchema (RPCResponse LSP.RespScript) where
  declareNamedSchema _ = do
    rsp <- declareSchemaRef (Proxy :: Proxy LSP.RespScript)
    rspOK <- declareSchemaRef (Proxy :: Proxy ())
    rspErr <- declareSchemaRef (Proxy :: Proxy RPCResponseError)
    pure $ NamedSchema (Just "RPCResponse Script")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("RPCResp", rsp), ("RPCRespError", rspErr)]
               }

instance ToSchema (RPCResponse LSP.RespMethod) where
  declareNamedSchema _ = do
    rsp <- declareSchemaRef (Proxy :: Proxy LSP.RespMethod)
    rspOK <- declareSchemaRef (Proxy :: Proxy ())
    rspErr <- declareSchemaRef (Proxy :: Proxy RPCResponseError)
    pure $ NamedSchema (Just "RPCResponse Method")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("RPCResp", rsp), ("RPCRespError", rspErr)]
               }

instance ToSchema (RPCResponse LSP.RespDef) where
  declareNamedSchema _ = do
    rsp <- declareSchemaRef (Proxy :: Proxy LSP.RespDef)
    rspOK <- declareSchemaRef (Proxy :: Proxy ())
    rspErr <- declareSchemaRef (Proxy :: Proxy RPCResponseError)
    pure $ NamedSchema (Just "RPCResponse Def")
      $ mempty { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
               , _schemaProperties = fromList [("RPCResp", rsp), ("RPCRespError", rspErr)]
               }

--------------------
-- Scripts
--------------------

type ScriptCompile resp = ReqBody '[JSON] LSP.ReqScript :> Post '[JSON] (resp LSP.RespScript)
type ScriptCompileRaw resp = "raw" :> ReqBody '[PlainText] Text :> Post '[JSON] (resp LSP.RespScript)
type ScriptParse resp = ReqBody '[JSON] LSP.ReqScript :> Post '[JSON] (resp LSP.RespScript)

type ScriptsAPI resp = "scripts" :>
 ( ("compile" :> (ScriptCompile resp :<|> ScriptCompileRaw resp)) :<|>
   ("parse" :> ScriptParse resp)
 )

scriptsCompile :: Monad m => LSP.ReqScript -> m (RPCResponse LSP.RespScript)
scriptsCompile req
  = case LSP.scriptCompile req of
      Left err -> pure . RPCRespError . RPCLSPErr $ err
      Right resp -> pure  . RPCResp $ resp

scriptsCompileRaw :: Monad m => Text -> m (RPCResponse LSP.RespScript)
scriptsCompileRaw body
  = case LSP.scriptCompileRaw body of
      Left err -> pure . RPCRespError . RPCLSPErr $ err
      Right script -> pure  . RPCResp $ script

scriptsParse :: Monad m => LSP.ReqScript -> m (RPCResponse LSP.RespScript)
scriptsParse req
 = case LSP.scriptParse req of
     Left err -> pure $ RPCRespError . RPCLSPErr $ err
     Right script -> pure $ RPCResp script

--------------------
-- Methods
--------------------

type MethodsCompileRaw resp = "raw" :> ReqBody '[PlainText] Text :> Post '[JSON] (resp LSP.RespMethod)
type MethodsCompile resp = ReqBody '[JSON] LSP.ReqMethod :> Post '[JSON] (resp LSP.RespMethod)

type MethodsAPI resp = "methods" :>
 ("compile" :> (MethodsCompile resp :<|> MethodsCompileRaw resp))

methodCompileRaw :: Monad m => Text -> m (RPCResponse LSP.RespMethod)
methodCompileRaw text
  = case LSP.methodCompileRaw text of
      Left err -> pure . RPCRespError . RPCLSPErr $ err
      Right method -> pure . RPCResp $ method

methodCompile :: Monad m => LSP.ReqMethod -> m (RPCResponse LSP.RespMethod)
methodCompile req
  = case LSP.methodCompile req of
        Left err -> pure . RPCRespError . RPCLSPErr $ err
        Right method -> pure . RPCResp $ method

--------------------
-- Defs
--------------------

type DefsCompileRaw resp = "raw" :> ReqBody '[PlainText] Text :> Post '[JSON] (resp LSP.RespDef)
type DefsCompile resp = ReqBody '[JSON] LSP.ReqDef :> Post '[JSON] (resp LSP.RespDef)

type DefsAPI resp = "defs" :>
  ("compile" :> (DefsCompile resp :<|> DefsCompileRaw resp))

defsCompileRaw :: Monad m => Text -> m (RPCResponse LSP.RespDef)
defsCompileRaw text = case LSP.defCompileRaw text of
  Left err -> pure . RPCRespError . RPCLSPErr $ err
  Right defn -> pure . RPCResp $ defn

defsCompile :: Monad m => LSP.ReqDef -> m (RPCResponse LSP.RespDef)
defsCompile req = case LSP.defCompile req of
  Left err -> pure . RPCRespError . RPCLSPErr $ err
  Right defn -> pure . RPCResp $ defn

----------------------
-- Server
----------------------

fclProxyAPI :: Proxy (FCLAPI RPCResponse)
fclProxyAPI = Proxy

api :: Proxy API
api = Proxy

fclServer :: Monad m => ServerT (FCLAPI RPCResponse) m
fclServer = scripts :<|> methods :<|> defs
  where
    scripts = (scriptsCompile :<|> scriptsCompileRaw) :<|> scriptsParse
    methods =  methodCompile :<|> methodCompileRaw
    defs = defsCompile :<|> defsCompileRaw

runHttpFclAsHandler :: (m a -> ExceptT ServantErr IO a) -> m a -> Handler a
runHttpFclAsHandler runner app  = Handler (runner app)

appToServer :: Server API
appToServer =
  swaggerSchemaUIServer (toSwagger fclProxyAPI)
    :<|> (hoistServer fclProxyAPI (runHttpFclAsHandler (flip runReaderT Config . runApp)) fclServer)

runHttpFcl :: IO ()
runHttpFcl = do
  let port = 8080
  putText $ "Running on port " <> show port
  run port . customCors . serve api $ appToServer
  where
    customCors = cors (const $ Just (simpleCorsResourcePolicy  { corsRequestHeaders = ["Accept", "Accept-Language", "Content-Language", "Content-Type"] }))

---------------------
-- Arbitrary
---------------------

instance Arbitrary a => Arbitrary (RPCResponse a) where
  arbitrary = oneof
    [ RPCResp <$> arbitrary
    , RPCRespError <$> arbitrary
    ]

instance Arbitrary RPCResponseError where
  arbitrary = RPCLSPErr <$> arbitrary
