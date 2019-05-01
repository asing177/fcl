module Main where

import qualified Data.Aeson.Encode.Pretty as A
import           Options.Applicative
import           Protolude
import qualified Script.Analysis          as Analysis (inferTransitions)
import qualified Script.Compile           as Compile
import qualified Script.Graphviz          as Graphviz
import qualified Script.Parser            as Parser (AddrParsers (..),
                                                     parseFile)
import qualified Script.Pretty            as Pretty
import qualified System.Exit
import qualified Utils


data ScriptCommand
  = CompileScript { file :: FilePath }
  | Lint { file :: FilePath }
  | Format { file :: FilePath }
  | Graph { file :: FilePath }
  | Transitions { file :: FilePath }
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

--------------------------------------------
-- Parser Utils
--------------------------------------------

fileParser :: Parser [Char]
fileParser = strArgument (metavar "FILE")

-- | CMD line driver for FCL repl
driverScript
  :: (Ord as, Ord ac, Ord c, Show as, Show ac, Show c, Pretty.Pretty as, Pretty.Pretty ac, Pretty.Pretty c)
  => Parser.AddrParsers as ac c
  -> ScriptCommand
  -> IO ()
driverScript addrParsers cmd = do
  e <- (flip runReaderT) addrParsers . runExceptT $
    case cmd of
    -- lint
    -- Lint scriptFile -> do
    --   res <- pure $ Compile.lintFile scriptFile
    --   case res of
    --     [] -> System.Exit.exitSuccess
    --     xs -> do
    --       putStrLn (A.encodePretty xs)
    --       System.Exit.exitFailure

    -- format
    Format scriptFile -> do
      script <- Compile.formatScript scriptFile
      pure $ putStrLn script

    -- graph
    Graph path -> do
      parsers <- ask
      pure $ Graphviz.fileWriteSVG parsers path

    -- compile
    CompileScript scriptFile -> do
      script <- Compile.compileFile scriptFile
      pure . putText . Pretty.prettyPrint $ script

    Transitions scriptFile -> do
      parsers <- ask
      pure $ do
        ast <- Parser.parseFile parsers scriptFile
        let transitions = Analysis.inferTransitions ast
            errs = Compile.transitionSoundness transitions
        putText "The following transitions were inferred:"
        putText ""
        putText $ Pretty.prettyPrint transitions
        putText ""
        case errs of
          []    -> Utils.putGreen $ Pretty.prettyPrint errs
          (_:_) -> Utils.putRed $ Pretty.prettyPrint errs -- don't fail

  case e of
    Left err -> print err
    Right x  -> x

main :: IO ()
main = putText $ "Hello FCL"
