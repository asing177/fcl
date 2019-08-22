{-# LANGUAGE PatternSynonyms #-}
module Language.FCL.SafeWorkflow.REPL
  ( module Language.FCL.SafeWorkflow.REPL
  ) where

import Protolude

import Data.Monoid (Dual(..))

import Control.Monad.RWS

import System.FilePath (FilePath, (</>))
import System.Directory (createDirectoryIfMissing)

import Language.FCL.Graphviz (workflowWriteSVG)
import Language.FCL.SafeWorkflow.Editable
  ( Continuation
  , EditLabel(..)
  , PrettyLabel(..)
  , EditableSW
  , pattern Hole
  , replaceHole
  , refreshTransitionIndices
  )

import qualified Language.FCL.SafeWorkflow.Editable as Edit

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
type History = Dual [EditableSW]

-- | Safe workflow REPL (read eval print loop) monad
-- Keeps track of the safe workflow being edited,
-- and has logging capabilities too.
type SWREPLM = RWS Options History EditableSW

runSWREPLPure :: SWREPLM a -> (a, EditableSW, History)
runSWREPLPure actionM = runRWS actionM defaultOpts (Hole 1)

runSWREPLWithLogging :: SWREPLM () -> IO ()
runSWREPLWithLogging
  = void
  . runSWREPLIOWithOpts (defaultOpts {loggingEnabled = True})

runSWREPLIOWithOpts :: Options -> SWREPLM a -> IO (a, EditableSW, History)
runSWREPLIOWithOpts opts@Options{..} actionM = do
  let r@(_, _, Dual history) = runRWS actionM opts (Hole 1)
      history' = Hole 1 : reverse history
  when loggingEnabled $ do
    createDirectoryIfMissing True logDirectory
    forM_ (zip history' [0..]) $ \(log, idx) -> do
      let filePath = logDirectory </> show idx
      printSW filePath log
  return r

replaceHoleM :: Int -> Continuation -> SWREPLM ()
replaceHoleM ix cont = do
  sw <- get
  let sw' = replaceHole (HLabel ix) cont sw
  tell $ Dual [sw']
  put sw'

printSW :: FilePath -> EditableSW -> IO ()
printSW path = workflowWriteSVG path . fmap PrettyLabel . refreshTransitionIndices

simpleWhiteBoardExample :: SWREPLM ()
simpleWhiteBoardExample = do
  replaceHoleM 1 (Edit.AND 2)
  replaceHoleM 1 Edit.Atom
  replaceHoleM 2 (Edit.XOR 2)
  replaceHoleM 1 Edit.Atom
  replaceHoleM 2 Edit.SimpleLoop
  replaceHoleM 1 Edit.Atom
  replaceHoleM 2 Edit.Atom
