{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Language.FCL.LanguageServerProtocol where

import Protolude

import Data.Aeson (ToJSON(..), FromJSON)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NE

import qualified Data.ByteString as BS
import qualified Language.FCL.AST as Script
import qualified Language.FCL.Analysis as Analysis
import qualified Language.FCL.Compile as Compile
import qualified Language.FCL.Graphviz as Graphviz
import qualified Language.FCL.Parser as Parser
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Typecheck as Typecheck
import Language.FCL.Warning (Warning(..))
import qualified Language.FCL.Effect as Effect
import qualified Language.FCL.Duplicate as Dupl
import qualified Language.FCL.SafeString as SafeString

-----------------------
-- Request datatypes --
-----------------------

data ReqScript
  = ReqScript
  { defs :: [ReqDef]
  , enums :: [ReqEnumDef]
  , methods :: [ReqMethod]
  , transitions :: [ReqTransition]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ReqTransition
  = ReqTransition
  { fromState :: Text
  , toState :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ReqMethod
  = ReqMethod
  { methodInputPlaces :: Script.WorkflowState
  , methodPreconditions :: Script.Preconditions
  , methodName :: Script.Name
  , methodBodyText :: Text
  , methodArgs :: [ReqMethodArg]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ReqMethodArg
  = ReqMethodArg
  { argName :: Script.Name
  , argType :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- TODO: Add other defs (GlobalDefNull)
data ReqDef
  = ReqGlobalDef
  { defName :: Text
  , defType :: Text
  , defValue :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ReqEnumDef
  = ReqEnumDef
  { enumName :: Script.Name
  , enumConstr :: [Script.EnumConstr]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data RespMethod
  = RespMethod
  { respMethod :: Script.Method
  , respPpMethod :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

parseMethod :: ReqMethod -> Either Parser.ParseErrInfo Script.Method
parseMethod  reqMethod@ReqMethod{..} = do
  parsedMethodBody <- Parser.parseBlock methodBodyText
  argTypes <- sequence $ (\a -> Parser.parseType (argType a)) <$> methodArgs
  let args = zipWith Script.Arg argTypes (Script.Located Script.NoLoc . argName <$> methodArgs)
  pure Script.Method
        { methodInputPlaces = methodInputPlaces
        , methodPreconditions = methodPreconditions
        , methodName = (Script.Located Script.NoLoc methodName)
        , methodBody = parsedMethodBody
        , methodArgs = args
        }

parseScript :: ReqScript -> Either Parser.ParseErrInfo Script.Script
parseScript reqScript@ReqScript{..} = do
  methods <- sequence $ parseMethod <$> methods
  defs <- sequence $ Parser.parseDefn . textifyReqDef <$> defs
  pure Script.Script
      { scriptDefs = defs
      , scriptEnums = (\e -> Script.EnumDef
                                (Script.Located Script.NoLoc (enumName e))
                                (Script.Located Script.NoLoc <$> (enumConstr e))
                             ) <$> enums
      , scriptMethods = methods
      , scriptTransitions = []
      , scriptHelpers = []
      }

compileScript :: ReqScript -> Either Compile.CompilationErr Compile.CheckedScript
compileScript reqScript@ReqScript{..} = do
  ast <- first Compile.ParseErr (parseScript reqScript)
  Compile.compileScript ast

data RespScript = RespScript
  { respScript :: Script.Script -- TODO: Keep method body text as it came
  , respPpScript :: Text
  , respScriptWarnings :: [Warning]
  , respScriptSigs :: [(Script.Name, Typecheck.Sig, Effect.Effects)]
  , respGraphviz :: Graphviz.Graphviz
  } deriving (Show, Generic, ToJSON, FromJSON)

validateScript :: ReqScript -> Either Compile.CompilationErr RespScript
validateScript reqScript@ReqScript{..} = do
  cs <- compileScript reqScript
  let script = Compile.checkedScript cs
  pure $ RespScript
    { respScript = script
    , respPpScript = toS $ Pretty.prettyPrint script
    , respScriptWarnings = Compile.checkedScriptWarnings cs
    , respScriptSigs = Compile.checkedScriptSigs cs
    , respGraphviz = Graphviz.methodsToGraphviz (Script.scriptMethods script)
    }

(<..>) :: Text -> Text -> Text
(<..>) t1 t2 = t1 <> " " <> t2

textifyReqDef :: ReqDef -> Text
textifyReqDef ReqGlobalDef{..} = defType <..> defName <..> "=" <..> defValue <> ";"

-- Language Server Protocol (LSP)
data LSP = LSP
  { startLineNumber :: Int
  , startColumn :: Int
  , endLineNumber :: Int
  , endColumn :: Int
  , message :: Text
  , severity :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

-- TODO: Return the type of the compilation error
toLSP :: Compile.CompilationErr -> [LSP]
toLSP cErr = case cErr of
  Compile.ParseErr Parser.ParseErrInfo{..}
    -> [LSP line column line endOfLine errMsg 8]
  Compile.DuplicationErr duplErrs
    -> (\dupErr -> case dupErr of
        Dupl.DuplicateMethod lname lexpr
          -> fullErrLSP (Script.located lname) (Text.length (Script.unName (Script.locVal lname))) dupErr
        Dupl.DuplicateFunction lname
          -> fullErrLSP (Script.located lname) (Text.length (Script.unName (Script.locVal lname))) dupErr
        Dupl.DuplicateConstructor l
          -> fullErrLSP (Script.located l) (BS.length (SafeString.toBytes $ Script.unEnumConstr (Script.locVal l))) dupErr
        Dupl.DuplicateEnumDef lname
          -> fullErrLSP (Script.located lname) (Text.length (Script.unName (Script.locVal lname))) dupErr
        Dupl.DuplicateVariable varA varB lname
          -> fullErrLSP (Script.located lname) (Text.length (Script.unName (Script.locVal lname))) dupErr
        -- TODO: Get location information for Duplicate Transition
        Dupl.DuplicateTransition transition
          -> noInfoLSP dupErr
        Dupl.DuplicatePrecondition (prec, lexpr)
          -> startErrLSP (Script.located lexpr) dupErr
       ) <$> duplErrs


  Compile.TypecheckErr neTypeErr
    -> (\te@Typecheck.TypeError{..} -> LSP
          { startLineNumber = Script.line errLoc
          , startColumn = Script.col errLoc
          , endLineNumber = Script.line errLoc
          , endColumn = endOfLine
          , message = Pretty.prettyPrint te
          , severity = 8
          }
       ) <$> NE.toList neTypeErr

  -- TODO: Get location information for transition errors
  Compile.TransitionErr transitionErrors@(Analysis.TransitionErrors _ errs)
    -> noInfoLSP <$> errs

  -- TODO: Get location information for workflow errors
  Compile.WorkflowErr workflowErrors
    -> noInfoLSP <$> workflowErrors

  -- TODO: Get location information for workflow errors
  Compile.UndefinednessErr undefErrors
    -> noInfoLSP <$> undefErrors

  Compile.EffectErr effectErrors
    -> (\case
          err@(Effect.LedgerEffectMismatch _ errLoc)
            -> startErrLSP errLoc err
          err@(Effect.PreconditionViolation{..})
            -> startErrLSP violationLocation err
          err@(Effect.HelperEffect{..})
            -> startErrLSP helperLocation err
          err@(Effect.OnlyReadEffectsAllowed{..})
            -> startErrLSP effectLocation err
          err@(Effect.PreconditionsEffect{..})
            -> startErrLSP preconditionLocation err
       ) <$> effectErrors

  where
    -- | Provide full information about the location of error
    fullErrLSP :: Pretty.Pretty err => Script.Loc -> Int -> err -> LSP
    fullErrLSP located len err
      = LSP
      { startLineNumber = Script.line located
      , startColumn = Script.col located
      , endLineNumber = Script.line located
      , endColumn = Script.col located + len
      , message = Pretty.prettyPrint err
      , severity = 8
      }

    -- | Do not provide location information about the error
    noInfoLSP :: Pretty.Pretty err => err -> LSP
    noInfoLSP err
      = LSP
      { startLineNumber = 0
      , startColumn = 0
      , endLineNumber = 0
      , endColumn = endOfLine
      , message = Pretty.prettyPrint err
      , severity = 8
      }

    -- | Only provide information about where the error begins
    startErrLSP :: Pretty.Pretty err => Script.Loc -> err -> LSP
    startErrLSP loc err
      = LSP
        { startLineNumber = Script.line loc
        , startColumn = Script.col loc
        , endLineNumber = Script.line loc
        , endColumn = endOfLine
        , message = Pretty.prettyPrint err
        , severity = 8
        }

    endOfLine :: Int
    endOfLine = 1000
