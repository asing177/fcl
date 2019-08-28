{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
module Language.FCL.SafeWorkflow.REPL
  ( module Language.FCL.SafeWorkflow.REPL
  ) where

import Protolude hiding (sequence)

import Data.List.List2
import Data.Monoid (Dual(..))

import Control.Monad.Gen
import Control.Monad.RWS

import System.FilePath (FilePath, (</>))
import System.Directory (createDirectoryIfMissing)

import Language.FCL.AST (Name, Script)
import Language.FCL.Graphviz (workflowWriteSVG)
import Language.FCL.SafeWorkflow.Editable
import Language.FCL.SafeWorkflow.CodeGen (codeGenScript)
import Language.FCL.Pretty (prettyPrint)

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
type SWREPLM = RWST Options History EditableSW (Gen TransId)

runSWREPLPure :: Options -> SWREPLM a -> (a, EditableSW, History)
runSWREPLPure opts actionM = runGen $ runRWST actionM opts (Hole 1)

runSWREPLWithLogging :: SWREPLM () -> IO ()
runSWREPLWithLogging
  = void
  . runSWREPLIOWithOpts (defaultOpts {loggingEnabled = True})

runSWREPLIOWithOpts :: Options -> SWREPLM a -> IO (a, EditableSW, History)
runSWREPLIOWithOpts opts@Options{..} actionM = do
  let r@(_, _, Dual history) = runSWREPLPure opts actionM
      history' = Hole 1 : reverse history
  when loggingEnabled $ do
    createDirectoryIfMissing True logDirectory
    forM_ (zip history' [0..]) $ \(log, idx) -> do
      let filePath = logDirectory </> show idx
      printSW filePath log
  return r

-- | Updates the stored workflow and logs the new result.
loggedModify :: (EditableSW -> EditableSW) -> SWREPLM ()
loggedModify transform = do
  sw <- get
  let sw' = transform sw
  tell $ Dual [sw']
  put sw'

-- | Finish a hole, replace it with a simple transition.
finish
  :: HoleId       -- ^ Identifier of hole to be replaced
  -> Name         -- ^ Name of the new transition
  -> SWREPLM ()
finish holeId atomName = do
  transId <- gen
  let cont = Edit.Atom (LFinished atomName transId)
  loggedModify (replaceHole holeId cont)

-- | Replace a hole with a parallel subworkflow.
parallel
  :: HoleId                 -- ^ Identifier of hole to be replaced
  -> Int                    -- ^ Number of parallel threads (AND branches)
  -> Name                   -- ^ Name of splitting transition
  -> Name                   -- ^ Name of joining transition
  -> List2 (Name, Name)     -- ^ Names for the places inside the AND-branches
  -> SWREPLM ()
parallel holeId numBranches splitName joinName names = do
  splitId <- gen
  joinId  <- gen
  let splitLabel = LFinished splitName splitId
      joinLabel  = LFinished joinName  joinId
  branchLabels <- forM names $ \(inName, outName) -> do
    inId  <- gen
    outId <- gen
    pure $ ANDBranchLabels (LPlace inName inId) (LPlace outName outId)
  let cont = Edit.AND splitLabel joinLabel branchLabels
  loggedModify (replaceHole holeId cont)

-- | Replace a hole with a branching subworkflow.
choice
  :: HoleId       -- ^ Identifier of hole to be replaced
  -> Int          -- ^ Number of possible choices (XOR branches)
  -> SWREPLM ()
choice holeId n = loggedModify (replaceHole holeId (Edit.XOR n))

-- | Replace a hole with a "stay-or-continue" construct.
-- Stay in the current state or progress forward.
stayOrContinue
  :: HoleId       -- ^ Identifier of hole to be replaced
  -> SWREPLM ()
stayOrContinue holeId = loggedModify (replaceHole holeId Edit.SimpleLoop)

-- | Replace a hole with a "loop-or-continue" construct.
-- Loop in the the current state with the option to exit.
loopOrContinue
  :: HoleId       -- ^ Identifier of hole to be replaced
  -> Name         -- ^ Name for the breakpoint
  -> SWREPLM ()
loopOrContinue holeId breakPointName = do
  breakPointId <- gen
  let cont = Edit.Loop (LPlace breakPointName breakPointId)
  loggedModify (replaceHole holeId cont)

-- | Replace a hole wth a sequence of two subworkflows.
sequence
  :: HoleId       -- ^ Identifier of hole to be replaced
  -> Name         -- ^ Name for the place in-between the the two subworklows
  -> SWREPLM ()
sequence holeId inbetweenName = do
  inbetweenId <- gen
  let cont = Edit.Seq (LPlace inbetweenName inbetweenId)
  loggedModify (replaceHole holeId cont)

-- TODO: See "ACF Continutation" note in Language.FCL.SafeWorkflow.Editable
acf
  :: HoleId       -- ^ Identifier of hole to be replaced
  -> SWREPLM ()
acf = panic "not implemented"

printSW :: FilePath -> EditableSW -> IO ()
printSW path = workflowWriteSVG path . prettify

execCodeGen :: SWREPLM a -> Script
execCodeGen = codeGenScript
            . snd3
            . runSWREPLPure defaultOpts
  where
    snd3 (_,x,_) = x

execCodeGenThenPrintScript :: SWREPLM a -> IO ()
execCodeGenThenPrintScript = putStr . prettyPrint . execCodeGen where

simpleWhiteBoardExample1 :: SWREPLM ()
simpleWhiteBoardExample1 = do
  parallel 1 2 "split" "join"
    [ ("lhsIn", "lhsOut")
    , ("rhsIn", "rhsOut")
    ]
  finish 1 "t1"
  choice 2 2
  finish 1 "t2"
  stayOrContinue 2
  finish 1 "t3"
  finish 2 "t4"

simpleWhiteBoardExample2 :: SWREPLM ()
simpleWhiteBoardExample2 = do
  parallel 1 2 "split" "join"
    [ ("lhsIn", "lhsOut")
    , ("rhsIn", "rhsOut")
    ]
  choice 2 2
  stayOrContinue 22
  finish 221 "t3"
  finish 222 "t4"
  finish 21  "t2"
  finish 1   "t1"

simpleWhiteBoardExample3 :: SWREPLM ()
simpleWhiteBoardExample3 = do
  parallel 1 2 "split" "join"
    [ ("lhsIn", "lhsOut")
    , ("rhsIn", "rhsOut")
    ]
  finish 1 "t1"
  choice 2 2
  finish 1 "t2"
  stayOrContinue 2
  finish 1 "t2"
  finish 2 "t2"
