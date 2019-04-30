{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Script.Command where

import           Protolude

import           Data.Aeson         (FromJSON, ToJSON (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as Text

import qualified Data.ByteString    as BS
import qualified SafeString
import qualified Script
import qualified Script.Analysis    as Analysis
import qualified Script.Compile     as Compile
import qualified Script.Duplicate   as Dupl
import qualified Script.Effect      as Effect
import qualified Script.Graphviz    as Graphviz
import qualified Script.Parser      as Parser
import qualified Script.Pretty      as Pretty
import qualified Script.Typecheck   as Typecheck
import           Script.Warning     (Warning (..))

-----------------------
-- Request datatypes --
-----------------------

data ReqScript as ac c
  = ReqScript
  { defs        :: [ReqDef]
  , enums       :: [ReqEnumDef]
  , methods     :: [ReqMethod as ac c]
  , transitions :: [ReqTransition]
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data ReqTransition
  = ReqTransition
  { fromState :: Text
  , toState   :: Text
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data ReqMethod as ac c
  = ReqMethod
  { methodInputPlaces   :: Script.WorkflowState
  , methodPreconditions :: Script.Preconditions as ac c
  , methodName          :: Script.Name
  , methodBodyText      :: Text
  , methodArgs          :: [ReqMethodArg]
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data ReqMethodArg
  = ReqMethodArg
  { argName :: Script.Name
  , argType :: Text
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

-- TODO: Add other defs (GlobalDefNull)
data ReqDef
  = ReqGlobalDef
  { defName  :: Text
  , defType  :: Text
  , defValue :: Text
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data ReqEnumDef
  = ReqEnumDef
  { enumName   :: Script.Name
  , enumConstr :: [Script.EnumConstr]
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data RespMethod as ac c
  = RespMethod
  { respMethod   :: Script.Method as ac c
  , respPpMethod :: Text
  } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

parseMethod :: ReqMethod as ac c-> ExceptT Parser.ParseErrInfo (Reader (Parser.AddrParsers as ac c)) (Script.Method as ac c)
parseMethod reqMethod@ReqMethod{..} = notImplemented
  -- = (\(parsedMethodBody, args) ->
  --     Script.Method
  --       { methodInputPlaces = methodInputPlaces
  --       , methodPreconditions = methodPreconditions
  --       , methodName = (Script.Located Script.NoLoc methodName)
  --       , methodBody = parsedMethodBody
  --       , methodArgs = args
  --       }
  --   ) <$> ( do
  --             addrParsers <- ask
  --             body <- Parser.parseBlock addrParsers methodBodyText
  --             argTypes <- sequence $ (\a -> Parser.parseType (argType a)) <$> methodArgs
  --             let args = zipWith Script.Arg argTypes (Script.Located Script.NoLoc . argName <$> methodArgs)
  --             pure (body, args)
  --         )

parseScript :: ReqScript as ac c -> Either Parser.ParseErrInfo (Script.Script as ac c)
parseScript reqScript@ReqScript{..} = notImplemented
  -- = (\(methods, defs) -> Script.Script
  --     { scriptDefs = defs
  --     , scriptEnums = (\e -> Script.EnumDef
  --                               (Script.Located Script.NoLoc (enumName e))
  --                               (Script.Located Script.NoLoc <$> (enumConstr e))
  --                            ) <$> enums
  --     , scriptMethods = methods
  --     , scriptTransitions = []
  --     , scriptHelpers = []
  --     }
  --   ) <$> (do
  --           methods <- sequence $ parseMethod <$> methods
  --           defs <- sequence $ Parser.parseDefn . textifyReqDef <$> defs
  --           pure (methods, defs)
  --         )

compileScript :: ReqScript as ac c -> Either (Compile.CompilationErr as ac c) (Compile.CheckedScript as ac c)
compileScript reqScript@ReqScript{..} = notImplemented
  -- do
  -- ast <- first Compile.ParseErr (parseScript reqScript)
  -- Compile.compileScript ast

data RespScript as ac c
  = RespScript
  { respScript         :: Script.Script as ac c -- TODO: Keep method body text as it came
  , respPpScript       :: Text
  , respScriptWarnings :: [Warning]
  , respScriptSigs     :: [(Script.Name, Typecheck.Sig, Effect.Effects)]
  , respGraphviz       :: Graphviz.Graphviz
  } deriving (Show, Generic, ToJSON, FromJSON)

validateScript :: ReqScript as ac c -> Either (Compile.CompilationErr as ac c) (RespScript as ac c)
validateScript reqScript@ReqScript{..} = notImplemented
  -- do
  -- cs <- compileScript reqScript
  -- let script = Compile.checkedScript cs
  -- pure $ RespScript
  --   { respScript = script
  --   , respPpScript = toS $ Pretty.prettyPrint script
  --   , respScriptWarnings = Compile.checkedScriptWarnings cs
  --   , respScriptSigs = Compile.checkedScriptSigs cs
  --   , respGraphviz = Graphviz.methodsToGraphviz (Script.scriptMethods script)
  --   }

(<..>) :: Text -> Text -> Text
(<..>) t1 t2 = t1 <> " " <> t2

textifyReqDef :: ReqDef -> Text
textifyReqDef ReqGlobalDef{..} = defType <..> defName <..> "=" <..> defValue <> ";"


-- Language Server Protocol (LSP)
data LSP = LSP
  { startLineNumber :: Int
  , startColumn     :: Int
  , endLineNumber   :: Int
  , endColumn       :: Int
  , message         :: Text
  , severity        :: Int
  } deriving (Show, Generic, NFData, ToJSON, FromJSON)

-- TODO: Return the type of the compilation error
-- TODO: Reduce duplication
toLSP :: Compile.CompilationErr as ac c -> [LSP]
toLSP cErr = notImplemented
-- case cErr of
--   Compile.ParseErr Parser.ParseErrInfo{..}
--     -> [LSP line column line endOfLine errMsg 8]
--   Compile.DuplicationErr duplErrs
--     -> (\dupErr -> case dupErr of
--         Dupl.DuplicateMethod lname lexpr
--           -> let loc = Script.located lname
--               in LSP
--                   { startLineNumber = Script.line loc
--                   , startColumn = Script.col loc
--                   , endLineNumber = Script.line loc
--                   , endColumn = Script.col loc + Text.length (Script.unName $ Script.locVal lname)
--                   , message = Pretty.prettyPrint dupErr
--                   , severity = 8
--                   }

--         Dupl.DuplicateFunction Script.Located{..}
--           -> LSP
--               { startLineNumber = Script.line located
--               , startColumn = Script.col located
--               , endLineNumber = Script.line located
--               , endColumn = Script.col located + Text.length (Script.unName locVal)
--               , message = Pretty.prettyPrint dupErr
--               , severity = 8
--               }

--         Dupl.DuplicateConstructor Script.Located{..}
--           -> LSP
--               { startLineNumber = Script.line located
--               , startColumn = Script.col located
--               , endLineNumber = Script.line located
--               , endColumn = Script.col located + BS.length (SafeString.toBytes $ Script.unEnumConstr locVal)
--               , message = Pretty.prettyPrint dupErr
--               , severity = 8
--               }

--         Dupl.DuplicateEnumDef Script.Located{..}
--           -> LSP
--               { startLineNumber = Script.line located
--               , startColumn = Script.col located
--               , endLineNumber = Script.line located
--               , endColumn = Script.col located + Text.length (Script.unName locVal)
--               , message = Pretty.prettyPrint dupErr
--               , severity = 8
--               }

--         Dupl.DuplicateVariable varA varB lname@Script.Located{..}
--           -> LSP
--               { startLineNumber = Script.line located
--               , startColumn = Script.col located
--               , endLineNumber = Script.line located
--               , endColumn = Script.col located + Text.length (Script.unName locVal)
--               , message = Pretty.prettyPrint dupErr
--               , severity = 8
--               }


--         -- TODO: Get location information for Duplicate Transition
--         Dupl.DuplicateTransition transition
--           -> LSP
--               { startLineNumber = 0
--               , startColumn = 0
--               , endLineNumber = 0
--               , endColumn = endOfLine
--               , message = Pretty.prettyPrint dupErr
--               , severity = 8
--               }

--         Dupl.DuplicatePrecondition (prec, lexpr@Script.Located{..})
--           -> LSP
--               { startLineNumber = Script.line located
--               , startColumn = Script.col located
--               , endLineNumber = Script.line located
--               , endColumn = endOfLine
--               , message = Pretty.prettyPrint dupErr
--               , severity = 8
--               }

--        ) <$> duplErrs


--   Compile.TypecheckErr neTypeErr
--     -> (\te@Typecheck.TypeError{..} -> LSP
--           { startLineNumber = Script.line errLoc
--           , startColumn = Script.col errLoc
--           , endLineNumber = Script.line errLoc
--           , endColumn = endOfLine
--           , message = Pretty.prettyPrint te
--           , severity = 8
--           }
--        ) <$> NE.toList neTypeErr

--   -- TODO: Get location information for transition errors
--   Compile.TransitionErr transitionErrors@(Analysis.TransitionErrors _ errs)
--     -> (\err -> LSP
--           { startLineNumber = 0
--           , startColumn = 0
--           , endLineNumber = 0
--           , endColumn = endOfLine
--           , message = Pretty.prettyPrint err
--           , severity = 8
--           }
--        ) <$> errs

--   -- TODO: Get location information for workflow errors
--   Compile.WorkflowErr workflowErrors
--     -> (\err -> LSP
--           { startLineNumber = 0
--           , startColumn = 0
--           , endLineNumber = 0
--           , endColumn = endOfLine
--           , message = Pretty.prettyPrint err
--           , severity = 8
--           }
--        ) <$> workflowErrors

--   -- TODO: Get location information for workflow errors
--   Compile.UndefinednessErr undefErrors
--     -> (\err -> LSP
--           { startLineNumber = 0
--           , startColumn = 0
--           , endLineNumber = 0
--           , endColumn = endOfLine
--           , message = Pretty.prettyPrint err
--           , severity = 8
--           }
--        ) <$> undefErrors

--   Compile.EffectErr effectErrors
--     -> (\case
--           err@(Effect.LedgerEffectMismatch _ errLoc)
--             -> LSP
--                 { startLineNumber = Script.line errLoc
--                 , startColumn = Script.col errLoc
--                 , endLineNumber = Script.line errLoc
--                 , endColumn = endOfLine
--                 , message = Pretty.prettyPrint err
--                 , severity = 8
--                 }
--           err@(Effect.PreconditionViolation{..})
--             -> LSP
--                 { startLineNumber = Script.line violationLocation
--                 , startColumn = Script.col violationLocation
--                 , endLineNumber = Script.line violationLocation
--                 , endColumn = endOfLine
--                 , message = Pretty.prettyPrint err
--                 , severity = 8
--                 }
--           err@(Effect.HelperEffect{..})
--             -> LSP
--                 { startLineNumber = Script.line helperLocation
--                 , startColumn = Script.col helperLocation
--                 , endLineNumber = Script.line helperLocation
--                 , endColumn = endOfLine
--                 , message = Pretty.prettyPrint err
--                 , severity = 8
--                 }
--           err@(Effect.CollectionEffect{..})
--             -> LSP
--                 { startLineNumber = Script.line collectionLocation
--                 , startColumn = Script.col collectionLocation
--                 , endLineNumber = Script.line collectionLocation
--                 , endColumn = endOfLine
--                 , message = Pretty.prettyPrint err
--                 , severity = 8
--                 }
--           err@(Effect.PreconditionsEffect{..})
--             -> LSP
--                 { startLineNumber = Script.line preconditionLocation
--                 , startColumn = Script.col preconditionLocation
--                 , endLineNumber = Script.line preconditionLocation
--                 , endColumn = endOfLine
--                 , message = Pretty.prettyPrint err
--                 , severity = 8
--                 }

--        ) <$> effectErrors

--   where
--     endOfLine :: Int
--     endOfLine = 1000



