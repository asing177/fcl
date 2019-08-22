module Language.FCL.SafeWorkflow.REPL
  ( module Language.FCL.SafeWorkflow.REPL
  ) where

import Protolude

import Data.Monoid (Dual(..))

import Control.Monad.RWS

import System.FilePath (FilePath, (</>))
import System.Directory (createDirectoryIfMissing)

import Language.FCL.Graphviz (Transitions(..), workflowWriteSVG)
import Language.FCL.SafeWorkflow (SafeWorkflow(..), constructTransitions)
import Language.FCL.SafeWorkflow.Simple (SimpleSafeWorkflow)
import Language.FCL.SafeWorkflow.DSL (Continuation, HLabel, replaceHole)

import qualified Language.FCL.SafeWorkflow.DSL as DSL

-- TODO: currently Options has no impact on any operation, refactor
-- | Options for running the safe workflow REPL
data Options = Options
  { logDirectory   :: FilePath
  , loggingEnabled :: Bool
  } deriving (Eq, Ord, Show)

-- | Default options for running the safe workflow REPL
defaultOpts :: Options
defaultOpts = Options
  { logDirectory   = "./logs"
  , loggingEnabled = False
  }

-- | Edit history for thesafe workflow REPL.
type History = Dual [SimpleSafeWorkflow]

-- | Safe workflow REPL (read eval print loop) monad
-- Keeps track of the safe workflow being edited,
-- and has logging capabilities too.
type SWREPLM = RWS Options History SimpleSafeWorkflow

runSWREPLPure :: SWREPLM a -> (a, SimpleSafeWorkflow, History)
runSWREPLPure actionM = runRWS actionM defaultOpts (Hole 1)

runSWREPLWithLogging :: SWREPLM () -> IO ()
runSWREPLWithLogging
  = void
  . runSWREPLIOWithOpts (defaultOpts {loggingEnabled = True})

runSWREPLIOWithOpts :: Options -> SWREPLM a -> IO (a, SimpleSafeWorkflow, History)
runSWREPLIOWithOpts opts@Options{..} actionM = do
  let r@(_, _, Dual history) = runRWS actionM opts (Hole 1)
      history' = Hole 1 : reverse history
  when loggingEnabled $ do
    createDirectoryIfMissing True logDirectory
    forM_ (zip history' [0..]) $ \(log, idx) -> do
      let filePath = logDirectory </> show idx
      printSW filePath log
  return r

replaceHoleM :: HLabel -> Continuation -> SWREPLM ()
replaceHoleM lbl cont = do
  sw <- get
  let sw' = replaceHole lbl cont sw
  tell $ Dual [sw']
  put sw'

printSW :: FilePath -> SimpleSafeWorkflow -> IO ()
printSW path = workflowWriteSVG path . Transitions . sort . constructTransitions

simpleWhiteBoardExample :: SWREPLM ()
simpleWhiteBoardExample = do
  replaceHoleM 1 (DSL.AND 2)
  replaceHoleM 1 DSL.Atom
  replaceHoleM 2 (DSL.XOR 2)
  replaceHoleM 1 DSL.Atom
  replaceHoleM 2 DSL.SimpleLoop
  replaceHoleM 1 DSL.Atom
  replaceHoleM 2 DSL.Atom
