{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Language.FCL.Command where

import Protolude

import Data.Aeson (ToJSON(..), FromJSON)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NE

import qualified Data.ByteString as BS
import qualified Script
import qualified Language.FCL.Analysis as Analysis
import qualified Language.FCL.Compile as Compile
import qualified Language.FCL.Graphviz as Graphviz
import qualified Language.FCL.Parser as Parser
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Typecheck as Typecheck
import Language.FCL.Warning (Warning(..))
import qualified Language.FCL.Effect as Effect
import qualified Language.FCL.Duplicate as Dupl
import qualified SafeString

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
  { methodInputPlaces :: Language.FCL.WorkflowState
  , methodPreconditions :: Language.FCL.Preconditions
  , methodName :: Language.FCL.Name
  , methodBodyText :: Text
  , methodArgs :: [ReqMethodArg]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ReqMethodArg
  = ReqMethodArg
  { argName :: Language.FCL.Name
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
  { enumName :: Language.FCL.Name
  , enumConstr :: [Language.FCL.EnumConstr]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


parseMethod :: ReqMethod -> Either Parser.ParseErrInfo Language.FCL.Method
parseMethod  reqMethod@ReqMethod{..}
  = (\(parsedMethodBody, args) ->
      Language.FCL.Method
        { methodInputPlaces = methodInputPlaces
        , methodPreconditions = methodPreconditions
        , methodName = (Language.FCL.Located Language.FCL.NoLoc methodName)
        , methodBody = parsedMethodBody
        , methodArgs = args
        }
    ) <$> ( do
              body <- Parser.parseBlock methodBodyText
              argTypes <- sequence $ (\a -> Parser.parseType (argType a)) <$> methodArgs
              let args = zipWith Language.FCL.Arg argTypes (Language.FCL.Located Language.FCL.NoLoc . argName <$> methodArgs)
              pure (body, args)
          )

parseScript :: ReqScript -> Either Parser.ParseErrInfo Language.FCL.Script
parseScript reqScript@ReqScript{..}
  = (\(methods, defs) -> Language.FCL.Script
      { scriptDefs = defs
      , scriptEnums = (\e -> Language.FCL.EnumDef
                                (Language.FCL.Located Language.FCL.NoLoc (enumName e))
                                (Language.FCL.Located Language.FCL.NoLoc <$> (enumConstr e))
                             ) <$> enums
      , scriptMethods = methods
      , scriptTransitions = []
      , scriptHelpers = []
      }
    ) <$> (do
            methods <- sequence $ parseMethod <$> methods
            defs <- sequence $ Parser.parseDefn . textifyReqDef <$> defs
            pure (methods, defs)
          )

compileScript :: ReqScript -> Either Compile.CompilationErr Compile.CheckedScript
compileScript reqScript@ReqScript{..} = do
  ast <- first Compile.ParseErr (parseScript reqScript)
  Compile.compileScript ast

data RespScript = RespScript
  { respScript :: Language.FCL.Script -- TODO: Keep method body text as it came
  , respPpScript :: Text
  , respScriptWarnings :: [Warning]
  , respScriptSigs :: [(Language.FCL.Name, Typecheck.Sig, Effect.Effects)]
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
    , respGraphviz = Graphviz.graphviz (Language.FCL.scriptMethods script)
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
-- TODO: Reduce duplication
toLSP :: Compile.CompilationErr -> [LSP]
toLSP cErr = case cErr of
  Compile.ParseErr Parser.ParseErrInfo{..}
    -> [LSP line column line endOfLine errMsg 8]
  Compile.DuplicationErr duplErrs
    -> (\dupErr -> case dupErr of
        Dupl.DuplicateMethod lname lexpr
          -> let loc = Language.FCL.located lname
              in LSP
                  { startLineNumber = Language.FCL.line loc
                  , startColumn = Language.FCL.col loc
                  , endLineNumber = Language.FCL.line loc
                  , endColumn = Language.FCL.col loc + Text.length (Language.FCL.unName $ Language.FCL.locVal lname)
                  , message = Pretty.prettyPrint dupErr
                  , severity = 8
                  }

        Dupl.DuplicateFunction Language.FCL.Located{..}
          -> LSP
              { startLineNumber = Language.FCL.line located
              , startColumn = Language.FCL.col located
              , endLineNumber = Language.FCL.line located
              , endColumn = Language.FCL.col located + Text.length (Language.FCL.unName locVal)
              , message = Pretty.prettyPrint dupErr
              , severity = 8
              }

        Dupl.DuplicateConstructor Language.FCL.Located{..}
          -> LSP
              { startLineNumber = Language.FCL.line located
              , startColumn = Language.FCL.col located
              , endLineNumber = Language.FCL.line located
              , endColumn = Language.FCL.col located + BS.length (SafeString.toBytes $ Language.FCL.unEnumConstr locVal)
              , message = Pretty.prettyPrint dupErr
              , severity = 8
              }

        Dupl.DuplicateEnumDef Language.FCL.Located{..}
          -> LSP
              { startLineNumber = Language.FCL.line located
              , startColumn = Language.FCL.col located
              , endLineNumber = Language.FCL.line located
              , endColumn = Language.FCL.col located + Text.length (Language.FCL.unName locVal)
              , message = Pretty.prettyPrint dupErr
              , severity = 8
              }

        Dupl.DuplicateVariable varA varB lname@Language.FCL.Located{..}
          -> LSP
              { startLineNumber = Language.FCL.line located
              , startColumn = Language.FCL.col located
              , endLineNumber = Language.FCL.line located
              , endColumn = Language.FCL.col located + Text.length (Language.FCL.unName locVal)
              , message = Pretty.prettyPrint dupErr
              , severity = 8
              }


        -- TODO: Get location information for Duplicate Transition
        Dupl.DuplicateTransition transition
          -> LSP
              { startLineNumber = 0
              , startColumn = 0
              , endLineNumber = 0
              , endColumn = endOfLine
              , message = Pretty.prettyPrint dupErr
              , severity = 8
              }

        Dupl.DuplicatePrecondition (prec, lexpr@Language.FCL.Located{..})
          -> LSP
              { startLineNumber = Language.FCL.line located
              , startColumn = Language.FCL.col located
              , endLineNumber = Language.FCL.line located
              , endColumn = endOfLine
              , message = Pretty.prettyPrint dupErr
              , severity = 8
              }

       ) <$> duplErrs


  Compile.TypecheckErr neTypeErr
    -> (\te@Typecheck.TypeError{..} -> LSP
          { startLineNumber = Language.FCL.line errLoc
          , startColumn = Language.FCL.col errLoc
          , endLineNumber = Language.FCL.line errLoc
          , endColumn = endOfLine
          , message = Pretty.prettyPrint te
          , severity = 8
          }
       ) <$> NE.toList neTypeErr

  -- TODO: Get location information for transition errors
  Compile.TransitionErr transitionErrors@(Analysis.TransitionErrors _ errs)
    -> (\err -> LSP
          { startLineNumber = 0
          , startColumn = 0
          , endLineNumber = 0
          , endColumn = endOfLine
          , message = Pretty.prettyPrint err
          , severity = 8
          }
       ) <$> errs

  -- TODO: Get location information for workflow errors
  Compile.WorkflowErr workflowErrors
    -> (\err -> LSP
          { startLineNumber = 0
          , startColumn = 0
          , endLineNumber = 0
          , endColumn = endOfLine
          , message = Pretty.prettyPrint err
          , severity = 8
          }
       ) <$> workflowErrors

  -- TODO: Get location information for workflow errors
  Compile.UndefinednessErr undefErrors
    -> (\err -> LSP
          { startLineNumber = 0
          , startColumn = 0
          , endLineNumber = 0
          , endColumn = endOfLine
          , message = Pretty.prettyPrint err
          , severity = 8
          }
       ) <$> undefErrors

  Compile.EffectErr effectErrors
    -> (\case
          err@(Effect.LedgerEffectMismatch _ errLoc)
            -> LSP
                { startLineNumber = Language.FCL.line errLoc
                , startColumn = Language.FCL.col errLoc
                , endLineNumber = Language.FCL.line errLoc
                , endColumn = endOfLine
                , message = Pretty.prettyPrint err
                , severity = 8
                }
          err@(Effect.PreconditionViolation{..})
            -> LSP
                { startLineNumber = Language.FCL.line violationLocation
                , startColumn = Language.FCL.col violationLocation
                , endLineNumber = Language.FCL.line violationLocation
                , endColumn = endOfLine
                , message = Pretty.prettyPrint err
                , severity = 8
                }
          err@(Effect.HelperEffect{..})
            -> LSP
                { startLineNumber = Language.FCL.line helperLocation
                , startColumn = Language.FCL.col helperLocation
                , endLineNumber = Language.FCL.line helperLocation
                , endColumn = endOfLine
                , message = Pretty.prettyPrint err
                , severity = 8
                }
          err@(Effect.CollectionEffect{..})
            -> LSP
                { startLineNumber = Language.FCL.line collectionLocation
                , startColumn = Language.FCL.col collectionLocation
                , endLineNumber = Language.FCL.line collectionLocation
                , endColumn = endOfLine
                , message = Pretty.prettyPrint err
                , severity = 8
                }
          err@(Effect.PreconditionsEffect{..})
            -> LSP
                { startLineNumber = Language.FCL.line preconditionLocation
                , startColumn = Language.FCL.col preconditionLocation
                , endLineNumber = Language.FCL.line preconditionLocation
                , endColumn = endOfLine
                , message = Pretty.prettyPrint err
                , severity = 8
                }

       ) <$> effectErrors

  where
    endOfLine :: Int
    endOfLine = 1000



