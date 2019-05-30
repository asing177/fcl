{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Language.FCL.AST as Script
import Language.FCL.Parser as Parser
import Language.FCL.Typecheck as Typecheck
import Language.FCL.Pretty as Pretty
import Language.FCL.Compile as Compile
import Language.FCL.Graphviz as Graphviz
import qualified Language.FCL.LanguageServerProtocol as LSP

import SwaggerSchema()

data Config = Config

newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServantErr m) a
  } deriving ( Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

type App = AppT IO

type AppAPI = ScriptsAPI :<|> MethodsAPI :<|> DefsAPI

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> AppAPI

--------------------
-- Scripts
--------------------

type ScriptCompile = ReqBody '[JSON] LSP.ReqScript :> Post '[JSON] LSP.RespScript
type ScriptCompileRaw = "raw" :> ReqBody '[PlainText] Text :> Post '[JSON] LSP.RespScript
type ScriptParse = ReqBody '[JSON] LSP.ReqScript :> Post '[JSON] LSP.RespScript

type ScriptsAPI = "scripts" :>
 ("compile" :> (ScriptCompile :<|> ScriptCompileRaw)) :<|>
 ("parse" :> ScriptParse)

scriptsCompile :: LSP.ReqScript -> App LSP.RespScript
scriptsCompile req
  = case LSP.validateScript req of
      Left err -> panic $ "Validation error"
      Right resp -> pure resp

scriptsCompileRaw :: Text -> App LSP.RespScript
scriptsCompileRaw body
  = case Compile.compile body of
        Left err -> do
          panic . show $ LSP.toLSP err
        Right cs -> do
          let script = Compile.checkedScript cs
          pure
            $ LSP.RespScript
              { respScript = script
              , respPpScript = toS $ Pretty.prettyPrint script
              , respScriptWarnings = Compile.checkedScriptWarnings cs
              , respScriptSigs = Compile.checkedScriptSigs cs
              , respGraphviz = Graphviz.methodsToGraphviz (Script.scriptMethods script)
              }

scriptsParse :: LSP.ReqScript -> App LSP.RespScript
scriptsParse req
 = case LSP.parseScript req of
     Left err -> panic $ show err
     Right script -> pure
       $ LSP.RespScript
            script
            (Pretty.prettyPrint script)
            []
            []
            (Graphviz.methodsToGraphviz (Script.scriptMethods script))

--------------------
-- Methods
--------------------

type MethodsCompile = ReqBody '[JSON] LSP.ReqMethod :> Post '[JSON] LSP.RespMethod
type MethodsCompileRaw = "raw" :> ReqBody '[PlainText] Text :> Post '[JSON] LSP.RespMethod
type MethodsParse = ReqBody '[JSON] LSP.ReqMethod :> Post '[JSON] LSP.RespMethod

type MethodsAPI = "methods" :>
 ("compile" :> (MethodsCompile :<|> MethodsCompileRaw)) :<|>
 ("parse" :> MethodsParse)

methodsCompile = notImplemented
methodsCompileRaw = notImplemented

methodsParse :: LSP.ReqMethod -> App LSP.RespMethod
methodsParse req
  = case LSP.parseMethod req of
        Left err -> panic $ show err
        Right method -> pure
          $ LSP.RespMethod method (toS $ Pretty.prettyPrint method)

--------------------
-- Defs
--------------------

type DefsCompile = ReqBody '[JSON] LSP.ReqDef :> Post '[JSON] Def
type DefsCompileRaw = ReqBody '[JSON] Text :> Post '[JSON] Def
type DefsParse = ReqBody '[JSON] LSP.ReqDef :> Post '[JSON] Def

type DefsAPI = "defs" :>
  ("compile" :> (DefsCompile :<|> DefsCompileRaw)) :<|>
  ("parse" :> DefsParse)

defsCompileRaw :: Text -> App Def
defsCompileRaw def
  = case Parser.parseDefn (toS def) of
      Left err -> panic $ show err
      Right defn
        -> case Typecheck.runSolverM . snd
                $ Typecheck.runInferM
                (Script.EnumInfo mempty mempty)
                Typecheck.emptyInferState
                (Typecheck.tcDefn defn) of
             Left err -> panic $ show (Pretty.ppr err)
             Right _ -> pure defn

defsCompile = notImplemented
defsParse = notImplemented

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
    methods =  (methodsCompile :<|> methodsCompileRaw) :<|> methodsParse
    defs =  (defsCompile :<|> defsCompileRaw) :<|> defsParse

runAppAsHandler :: Config -> App a -> Handler a
runAppAsHandler cfg app = Handler $ runReaderT (runApp app) cfg

appToServer :: Config -> Server API
appToServer cfg =
  swaggerSchemaUIServer (toSwagger appAPI)
    :<|> (hoistServer appAPI (runAppAsHandler cfg) server)

runAPI :: IO ()
runAPI = do
  putText "Running on port 8080"
  run 8080 . serve api $ (appToServer Config)
