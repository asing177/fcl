module Main where

import qualified Data.Aeson.Encode.Pretty as A
import Options.Applicative
import Protolude
import qualified Language.FCL.Reachability.General as Reachability (completeReachabilityGraph, pprReachabilityGraph)
import qualified Language.FCL.Analysis as Analysis (inferTransitions)
import qualified Language.FCL.Undefinedness as Undefinedness (undefinednessAnalysis, fastUndefinednessAnalysis)
import qualified Language.FCL.Compile as Compile
import qualified Language.FCL.Graphviz as Graphviz
import qualified Language.FCL.Parser as Parser (parseFile)
import qualified Language.FCL.Pretty            as Pretty
import qualified Language.FCL.Utils as Utils
import qualified System.Exit

import qualified Data.Set as S

data ScriptCommand
  = CompileScript { file :: FilePath }
  | Lint { file :: FilePath }
  | Format { file :: FilePath }
  | Graph { file :: FilePath }
  | Transitions { file :: FilePath }
  | ReachabilityGraph { file :: FilePath }
  | UndefinednessAnalysis { file :: FilePath }
  | FastUndefinednessAnalysis { file :: FilePath }
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Script Commands
-------------------------------------------------------------------------------

scriptParser :: Parser ScriptCommand
scriptParser =
  scriptCompileParser
  <|> scriptFormat
  <|> scriptLint
  <|> scriptGraph
  <|> scriptTransitions
  <|> scriptReachabilityGraph
  <|> scriptUndefinednessAnalysis
  <|> scriptFastUndefinednessAnalysis

scriptFormat :: Parser ScriptCommand
scriptFormat = subparser $ command "format"
    (info (helper <*> scriptParser')
    (progDesc "Format a script"))
  where
    scriptParser' :: Parser ScriptCommand
    scriptParser' = Format <$> fileParser

    inplace :: Parser Bool
    inplace = flag False True $
         long "inplace"
      <> short 'i'
      <> help "Modify file in place"


scriptCompileParser :: Parser ScriptCommand
scriptCompileParser = subparser $ command "compile"
    (info (helper <*> scriptParser')
    (progDesc "Compile and typecheck a script."))
  where
    scriptParser' :: Parser ScriptCommand
    scriptParser' =
      CompileScript <$> fileParser

scriptLint :: Parser ScriptCommand
scriptLint = subparser $ command "lint"
    (info (helper <*> scriptParser')
    (progDesc "Lint a script."))
  where
    scriptParser' :: Parser ScriptCommand
    scriptParser' = Lint <$> fileParser

scriptGraph :: Parser ScriptCommand
scriptGraph = subparser $ command "graph"
    (info (helper <*> scriptParser')
    (progDesc "Extract graph from a script."))
  where
    scriptParser' :: Parser ScriptCommand
    scriptParser' = Graph <$> fileParser

scriptTransitions :: Parser ScriptCommand
scriptTransitions = subparser $ command "transitions"
    (info (helper <*> scriptParser')
    (progDesc "Infer the transition declarations of a script."))
  where
    scriptParser' :: Parser ScriptCommand
    scriptParser' = Transitions <$> fileParser

scriptReachabilityGraph :: Parser ScriptCommand
scriptReachabilityGraph = subparser $ command "reachability"
    (info (helper <*> scriptParser')
    (progDesc "Calculate the reachabality graph"))
  where
    scriptParser' :: Parser ScriptCommand
    scriptParser' = ReachabilityGraph <$> fileParser

scriptUndefinednessAnalysis :: Parser ScriptCommand
scriptUndefinednessAnalysis = subparser $ command "undefinedness"
    (info (helper <*> scriptParser')
    (progDesc "Run the undefinedness analysis"))
  where
    scriptParser' :: Parser ScriptCommand
    scriptParser' = UndefinednessAnalysis <$> fileParser

-- TODO: instead of this, add the "fast" flag to "undefinedness"
scriptFastUndefinednessAnalysis :: Parser ScriptCommand
scriptFastUndefinednessAnalysis = subparser $ command "fast-undefinedness"
    (info (helper <*> scriptParser')
    (progDesc "Run the fast undefinedness analysis"))
  where
    scriptParser' :: Parser ScriptCommand
    scriptParser' = FastUndefinednessAnalysis <$> fileParser

--------------------------------------------
-- Parser Utils
--------------------------------------------

fileParser :: Parser [Char]
fileParser = strArgument (metavar "FILE")

-- | CMD line driver for FCL repl
driverScript
  :: ScriptCommand
  -> IO ()
driverScript cmd
  = case cmd of
    -- lint
    Lint scriptFile -> do
      res <- Compile.lintFile scriptFile
      case res of
        [] -> System.Exit.exitSuccess
        xs -> do
          putStrLn (A.encodePretty xs)
          System.Exit.exitFailure

    -- format
    Format scriptFile -> do
      res <- Compile.formatScript scriptFile
      case res of
        Left err -> die err
        Right script -> putStrLn script

    -- graph
    Graph path ->
      Graphviz.fileWriteSVG path

    -- compile
    CompileScript scriptFile -> do
      Compile.compileFile scriptFile >>= \case
        Left err -> die err
        Right checked -> do
          putText . Pretty.prettyPrint $ checked

    Transitions scriptFile -> do
      ast <- Parser.parseFile scriptFile
      let transitions = Analysis.inferTransitions ast
          errs = Compile.transitionSoundness transitions
      putText "The following transitions were inferred:"
      putText ""
      putText $ Pretty.prettyPrint transitions
      putText ""
      case errs of
        [] -> Utils.putGreen $ Pretty.prettyPrint errs
        (_:_) -> die $ Pretty.prettyPrint errs

    ReachabilityGraph scriptFile -> do
      ast <- Parser.parseFile scriptFile
      let transitions = S.fromList $ Analysis.inferTransitions ast
          (errSet, rGraph) = Reachability.completeReachabilityGraph transitions
          errs = S.toList errSet
      putText "Reachability graph of the workflow net:"
      putText ""
      putText $ show $ Reachability.pprReachabilityGraph rGraph
      putText ""
      case errs of
        [] -> Utils.putGreen $ Pretty.prettyPrint errs
        (_:_) -> die $ Pretty.prettyPrint errs

    UndefinednessAnalysis scriptFile -> do
      ast <- Parser.parseFile scriptFile
      let undefRes = Undefinedness.undefinednessAnalysis ast
      case undefRes of
        Right _ -> Utils.putGreen "The workflow has no undefinedness errors."
        Left invalidStackTraces -> do
          die $ show $ Pretty.ppr invalidStackTraces

    FastUndefinednessAnalysis scriptFile -> do
      ast <- Parser.parseFile scriptFile
      case Undefinedness.fastUndefinednessAnalysis ast of
        Nothing -> Utils.putGreen "The workflow has no undefinedness errors."
        Just invalidStackTrace -> do
          die $ show $ Pretty.ppr invalidStackTrace


-------------------------------------------------------------------------------
-- Entry
-------------------------------------------------------------------------------

-- Command options
coptions :: ParserInfo ScriptCommand
coptions = info (helper <*> scriptParser) idm

cprefs :: ParserPrefs
cprefs = prefs showHelpOnError

-- Parse commmand line flags and handle application
main :: IO ()
main = customExecParser cprefs coptions >>= driverScript
