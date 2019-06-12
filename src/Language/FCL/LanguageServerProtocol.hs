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

import qualified Language.FCL.AST as AST
import qualified Language.FCL.Analysis as Analysis
import qualified Language.FCL.Compile as Compile
import qualified Language.FCL.Graphviz as Graphviz
import qualified Language.FCL.Parser as Parser
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Typecheck as Typecheck
import Language.FCL.Warning (Warning(..))
import qualified Language.FCL.Effect as Effect
import qualified Language.FCL.Duplicate as Dupl

-----------------------
-- Request datatypes --
-----------------------

data ReqScript
  = ReqScript
  { defs :: [ReqDef]
  , adts :: [ReqADTDef]
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
  { methodInputPlaces :: AST.WorkflowState
  , methodPreconditions :: AST.Preconditions
  , methodName :: AST.Name
  , methodBodyText :: Text
  , methodArgs :: [ReqMethodArg]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ReqMethodArg
  = ReqMethodArg
  { argName :: AST.Name
  , argType :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- TODO: Add other defs (GlobalDefNull)
data ReqDef
  = ReqGlobalDef
  { defName :: Text
  , defType :: Text
  , defValue :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ReqADTDef
  = ReqADTDef
  { adtName :: AST.NameUpper
  , adtConstr :: [AST.ADTConstr]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data RespMethod
  = RespMethod
  { respMethod :: AST.Method
  , respPpMethod :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

parseMethod :: ReqMethod -> Either Parser.ParseErrInfo AST.Method
parseMethod  reqMethod@ReqMethod{..} = do
  parsedMethodBody <- Parser.parseBlock methodBodyText
  argTypes <- sequence $ (\a -> Parser.parseType (argType a)) <$> methodArgs
  let args = zipWith AST.Arg argTypes (AST.Located AST.NoLoc . argName <$> methodArgs)
  pure AST.Method
        { methodInputPlaces = methodInputPlaces
        , methodPreconditions = methodPreconditions
        , methodName = (AST.Located AST.NoLoc methodName)
        , methodBody = parsedMethodBody
        , methodArgs = args
        }

parseScript :: ReqScript -> Either Parser.ParseErrInfo AST.Script
parseScript reqScript@ReqScript{..} = do
  methods <- sequence $ parseMethod <$> methods
  defs <- sequence $ Parser.parseDefn . textifyReqDef <$> defs
  pure AST.Script
      { scriptDefs = defs
      , scriptADTs = (\e -> AST.ADTDef
                                (AST.Located AST.NoLoc (adtName e))
                                (adtConstr e)
                             ) <$> adts
      , scriptMethods = methods
      , scriptTransitions = []
      , scriptHelpers = []
      }

compileScript :: ReqScript -> Either Compile.CompilationErr Compile.CheckedScript
compileScript reqScript@ReqScript{..} = do
  ast <- first Compile.ParseErr (parseScript reqScript)
  Compile.compileScript ast

data RespScript = RespScript
  { respScript :: AST.Script -- TODO: Keep method body text as it came
  , respPpScript :: Text
  , respScriptWarnings :: [Warning]
  , respScriptSigs :: [(AST.Name, Typecheck.Sig, Effect.Effects)]
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
    , respGraphviz = Graphviz.methodsToGraphviz (AST.scriptMethods script)
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
          -> fullErrLSP (AST.located lname) (Text.length (AST.unName (AST.locVal lname))) dupErr
        Dupl.DuplicateFunction lname
          -> fullErrLSP (AST.located lname) (Text.length (AST.unName (AST.locVal lname))) dupErr
        Dupl.DuplicateConstructor (AST.ADTConstr (AST.Located loc (AST.MkNameUpper nm)) _)
          -> fullErrLSP loc (Text.length nm) dupErr
        Dupl.DuplicateADTDef (AST.Located loc (AST.MkNameUpper nm))
          -> fullErrLSP loc (Text.length nm) dupErr
        Dupl.DuplicateVariable varA varB lname
          -> fullErrLSP (AST.located lname) (Text.length (AST.unName (AST.locVal lname))) dupErr
        -- TODO: Get location information for Duplicate Transition
        Dupl.DuplicateTransition transition
          -> noInfoLSP dupErr
        Dupl.DuplicatePrecondition (prec, lexpr)
          -> startErrLSP (AST.located lexpr) dupErr
        Dupl.DuplicateField lname
          -> fullErrLSP (AST.located lname) (Text.length (AST.unName (AST.locVal lname))) dupErr
       ) <$> duplErrs


  Compile.TypecheckErr neTypeErr
    -> (\te@Typecheck.TypeError{..} -> LSP
          { startLineNumber = AST.line errLoc
          , startColumn = AST.col errLoc
          , endLineNumber = AST.line errLoc
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
    fullErrLSP :: Pretty.Pretty err => AST.Loc -> Int -> err -> LSP
    fullErrLSP located len err
      = LSP
      { startLineNumber = AST.line located
      , startColumn = AST.col located
      , endLineNumber = AST.line located
      , endColumn = AST.col located + len
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
    startErrLSP :: Pretty.Pretty err => AST.Loc -> err -> LSP
    startErrLSP loc err
      = LSP
        { startLineNumber = AST.line loc
        , startColumn = AST.col loc
        , endLineNumber = AST.line loc
        , endColumn = endOfLine
        , message = Pretty.prettyPrint err
        , severity = 8
        }

    endOfLine :: Int
    endOfLine = 1000
