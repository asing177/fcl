{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Language.FCL.LanguageServerProtocol
  ( ReqScript(..)
  , ReqMethod(..)
  , ReqDef(..)
  , RespScript(..)
  , RespMethod(..)
  , RespDef(..)
  , ReqEnumDef(..)
  , ReqTransition(..)
  , ReqMethodArg(..)
  , scriptCompile
  , scriptCompileRaw
  , scriptParse
  , methodCompile
  , methodCompileRaw
  , defCompile
  , defCompileRaw
  , LSPErr(..)
  , LSP(..)
  , toLSP
  ) where

import Protolude

import Data.Aeson as A
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NE
import Text.PrettyPrint.Leijen.Text as PP hiding (Pretty, equals, (<$>))

import qualified Data.ByteString as BS
import qualified Language.FCL.AST as Script
import qualified Language.FCL.Analysis as Analysis
import qualified Language.FCL.Compile as Compile
import qualified Language.FCL.Graphviz as Graphviz
import qualified Language.FCL.Parser as Parser
import Language.FCL.Pretty as Pretty
import qualified Language.FCL.Typecheck as Typecheck
import qualified Language.FCL.Token as Token

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

data ReqDef
  = ReqGlobalDef
    { defType :: Text
    , defName :: Text
    , defValue :: Text
    }
  | ReqGlobalDefNull
    { defNullType :: Text
    , defNullName :: Text
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance Pretty.Pretty ReqDef where
  ppr (ReqGlobalDef typ name val)
    = case Parser.parseDecimal val of
        Left _ -> hsep [token Token.global, ppr typ, ppr name `assign` ppshow val]
        Right d -> hsep [token Token.global, ppr typ, ppr name `assign` ppr d]
  ppr (ReqGlobalDefNull typ name)
    = hsep [token Token.global, ppr typ, ppr name]  <> token Token.semi

data ReqEnumDef
  = ReqEnumDef
  { enumName :: Script.Name
  , enumConstr :: [Script.EnumConstr]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

------------------------
-- Response datatypes --
------------------------

data RespMethod
  = RespMethod
  { respMethod :: Script.Method
  , respPpMethod :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data RespScript = RespScript
  { respScript :: Script.Script
  , respPpScript :: Text
  , respScriptWarnings :: [Warning]
  , respScriptSigs :: [(Script.Name, Typecheck.Sig, Effect.Effects)]
  , respGraphviz :: Graphviz.Graphviz
  } deriving (Show, Generic, ToJSON, FromJSON)

data RespDef = RespDef
  { respDef :: Script.Def
  , respPpDef :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-------------
-- Scripts --
-------------

scriptCompile :: ReqScript -> Either LSPErr RespScript
scriptCompile req@ReqScript{..} = do
  ast <- respScript <$> scriptParse req
  cs <- first toLSPErr (Compile.compileScript ast)
  let script = Compile.checkedScript cs
  pure $ RespScript
    { respScript = script
    , respPpScript = toS $ Pretty.prettyPrint script
    , respScriptWarnings = Compile.checkedScriptWarnings cs
    , respScriptSigs = Compile.checkedScriptSigs cs
    , respGraphviz = Graphviz.methodsToGraphviz (Script.scriptMethods script)
    }

scriptCompileRaw :: Text -> Either LSPErr RespScript
scriptCompileRaw text
  = first toLSPErr $ Compile.compile text >>= \cs -> do
  let script = Compile.checkedScript cs
  pure $ RespScript
    { respScript = script
    , respPpScript = toS $ Pretty.prettyPrint script
    , respScriptWarnings = Compile.checkedScriptWarnings cs
    , respScriptSigs = Compile.checkedScriptSigs cs
    , respGraphviz = Graphviz.methodsToGraphviz (Script.scriptMethods script)
    }

scriptParse :: ReqScript -> Either LSPErr RespScript
scriptParse reqScript@ReqScript{..} = do
  methods <- sequence $ methodCompile <$> methods
  defs <- first (toLSPErr . Compile.ParseErr) (sequence $ Parser.parseDefn . Pretty.prettyPrint <$> defs)
  let enums' = (\e -> Script.EnumDef
                   (Script.Located Script.NoLoc (enumName e))
                   (Script.Located Script.NoLoc <$> (enumConstr e))
                 ) <$> enums
  let script = Script.Script enums' defs [] (respMethod <$> methods) []
  pure $ RespScript
           script
           (Pretty.prettyPrint script)
           []
           []
           (Graphviz.methodsToGraphviz (Script.scriptMethods script))

-------------
-- Methods --
-------------

methodCompile :: ReqMethod -> Either LSPErr RespMethod
methodCompile  reqMethod@ReqMethod{..} = do
  parsedMethodBody <- first (toLSPErr . Compile.ParseErr) (Parser.parseBlock methodBodyText)
  argTypes <- first (toLSPErr . Compile.ParseErr) (sequence $ (Parser.parseType . argType) <$> methodArgs)
  let args = zipWith Script.Arg argTypes (Script.Located Script.NoLoc . argName <$> methodArgs)
  let method = Script.Method
        { methodInputPlaces = methodInputPlaces
        , methodPreconditions = methodPreconditions
        , methodName = (Script.Located Script.NoLoc methodName)
        , methodBody = parsedMethodBody
        , methodArgs = args
        }
  pure $ RespMethod method (Pretty.prettyPrint method)


methodCompileRaw :: Text -> Either LSPErr RespMethod
methodCompileRaw text
  = first (toLSPErr . Compile.ParseErr) $ do
  method <- (Parser.parseMethod text)
  pure $ RespMethod method text

----------
-- Defs --
----------

defCompile :: ReqDef -> Either LSPErr RespDef
defCompile def = defCompileRaw (Pretty.prettyPrint def)

defCompileRaw :: Text -> Either LSPErr RespDef
defCompileRaw text = do
  defn <- first (toLSPErr . Compile.ParseErr) (Parser.parseDefn text)
  first (toLSPErr . Compile.TypecheckErr) (Typecheck.runSolverM . snd
                $ Typecheck.runInferM
                (Script.EnumInfo mempty mempty)
                Typecheck.emptyInferState
                (Typecheck.tcDefn defn))
  pure $ RespDef defn (Pretty.prettyPrint defn)

(<..>) :: Text -> Text -> Text
(<..>) t1 t2 = t1 <> " " <> t2

data LSPErr = LSPErr
  { lsp :: [LSP]
  , err :: Compile.CompilationErr
  } deriving (Show, Generic, FromJSON)

instance ToJSON LSPErr where
  toJSON (LSPErr{..}) = object
    [ "lsp"      .= toJSON lsp
    , "errorMsg" .= (Pretty.prettyPrint $ err :: Text)
    ]

toLSPErr :: Compile.CompilationErr -> LSPErr
toLSPErr err = LSPErr (toLSP err) err

-- Language Server Protocol (LSP)
data LSP = LSP
  { startLineNumber :: Int
  , startColumn :: Int
  , endLineNumber :: Int
  , endColumn :: Int
  , message :: Text
  , severity :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

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
