{-# LANGUAGE FlexibleContexts #-}

module Test.Workflow.Compilation (compilationTests) where

import Protolude

import Control.Monad ((>=>))
import qualified Data.Text as Text (unpack, pack)
import System.FilePath (replaceExtension)

import Language.FCL.Compile (compileFile)
import Language.FCL.Graphviz (callDot, fileToGraphviz)

import Test.Golden

-- To run only these tests and accept changes (use git diff to see what changed):
-- $ stack test --test-arguments='--pattern "Compilation" --accept' --fast
compilationTests :: IO TestTree
compilationTests = do
  positive <- discoverGoldenTestsFCL
    "tests/workflow/positive"
    (expectSuccess compileFile)
  negative <- discoverGoldenTestsFCL
    "tests/workflow/negative"
    (expectFailure compileFile)
  graphviz <- discoverGoldenTests
    [".s",".fcl"]
    ".dot"
    (\path gv -> do
      writeFile (replaceExtension path ".dot") (toS gv)
      writeFile (replaceExtension path ".svg") =<< toS . Text.unpack <$> callDot (Text.pack gv)
      )
    "tests/workflow"
    (fileToGraphviz >=> pure . Text.unpack)
  pure $ testGroup "Compilation" [positive, negative, graphviz]
