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
  , ReqADTDef(..)
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

import qualified Language.FCL.AST as AST
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

-----------------------
-- Request datatypes --
-----------------------

data ReqScript
  = ReqScript
  { defs :: [ReqDef]
  , adts :: [ReqADTDef]
  , methods :: [ReqMethod]
  , transitions :: [ReqTransition]
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON ReqScript where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON ReqScript where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

data ReqTransition
  = ReqTransition
  { fromState :: Text
  , toState :: Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON ReqTransition where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON ReqTransition where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

data ReqMethod
  = ReqMethod
  { methodInputPlaces :: AST.WorkflowState
  , methodPreconditions :: AST.Preconditions
  , methodName :: AST.Name
  , methodBodyText :: Text
  , methodArgs :: [ReqMethodArg]
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON ReqMethod where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON ReqMethod where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

data ReqMethodArg
  = ReqMethodArg
  { argName :: AST.Name
  , argType :: Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON ReqMethodArg where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON ReqMethodArg where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

data ReqDef
  = ReqDef
    { defType :: Text
    , defName :: Text
    , defValue :: Text
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON ReqDef where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON ReqDef where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Pretty.Pretty ReqDef where
  ppr (ReqDef typ name val) = case Parser.parseDecimal val of
        Left _ -> hsep [token Token.global, ppr typ, ppr name `assign` ppshow val]
        Right d -> hsep [token Token.global, ppr typ, ppr name `assign` ppr d]

data ReqADTDef
  = ReqADTDef
  { adtName :: AST.NameUpper
  , adtConstr :: [AST.ADTConstr]
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON ReqADTDef where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON ReqADTDef where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

------------------------
-- Response datatypes --
------------------------

data RespMethod
  = RespMethod
  { respMethod :: AST.Method
  , respPpMethod :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON RespMethod where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON RespMethod where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

data RespScript = RespScript
  { respScript :: AST.Script -- TODO: Keep method body text as it came
  , respPpScript :: Text
  , respScriptWarnings :: [Warning]
  , respScriptSigs :: [(AST.Name, Typecheck.Sig, Effect.Effects)]
  , respGraphviz :: Graphviz.Graphviz
  } deriving (Show, Generic)

instance ToJSON RespScript where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON RespScript where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

data RespDef = RespDef
  { respDef :: AST.Def
  , respPpDef :: Text
  } deriving (Show, Generic)

instance ToJSON RespDef where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON RespDef where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

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
    , respGraphviz = Graphviz.methodsToGraphviz (AST.scriptMethods script)
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
    , respGraphviz = Graphviz.methodsToGraphviz (AST.scriptMethods script)
    }

scriptParse :: ReqScript -> Either LSPErr RespScript
scriptParse reqScript@ReqScript{..} = do
  methods <- sequence $ methodCompile <$> methods
  defs <- first (toLSPErr . Compile.ParseErr) (sequence $ Parser.parseDefn . Pretty.prettyPrint <$> defs)
  let adts' = (\e -> AST.ADTDef
                                (AST.Located AST.NoLoc (adtName e))
                                (adtConstr e)
                             ) <$> adts
  let script = AST.Script adts' defs [] (respMethod <$> methods) []
  pure $ RespScript
           script
           (Pretty.prettyPrint script)
           []
           []
           (Graphviz.methodsToGraphviz (AST.scriptMethods script))

-------------
-- Methods --
-------------

methodCompile :: ReqMethod -> Either LSPErr RespMethod
methodCompile  reqMethod@ReqMethod{..} = do
  parsedMethodBody <- first (toLSPErr . Compile.ParseErr) (Parser.parseBlock methodBodyText)
  argTypes <- first (toLSPErr . Compile.ParseErr) (sequence $ (Parser.parseType . argType) <$> methodArgs)
  let args = zipWith AST.Arg argTypes (AST.Located AST.NoLoc . argName <$> methodArgs)
  let method = AST.Method
        { methodInputPlaces = methodInputPlaces
        , methodPreconditions = methodPreconditions
        , methodName = (AST.Located AST.NoLoc methodName)
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
                (AST.ADTInfo mempty mempty)
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
  } deriving (Show, Generic)

instance ToJSON LSP where
  toJSON = genericToJSON defaultOptions

instance FromJSON LSP where
  parseJSON = genericParseJSON defaultOptions

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
