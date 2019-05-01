{-# LANGUAGE LambdaCase #-}

module TestWarning where

import           Prelude           ()
import           Protolude

import qualified Reference         as Ref
import           Script.Compile    (CheckedScript (..), compileFile)
import           Script.Pretty     (prettyPrint)
import           Test.Tasty
import           Test.Tasty.Golden

runCompile :: ExceptT e (ReaderT Ref.CustomParsers IO) a -> IO (Either e a)
runCompile = (flip runReaderT) Ref.customAddrParsers . runExceptT

inPath, outPath :: FilePath -> FilePath
inPath fileName = "test/scripts/warning/" <> fileName <> ".s"
outPath fileName = "test/golden/warning/" <> fileName <> ".out"

warningTests :: TestTree
warningTests
  = testGroup "Script warning golden tests"
      [ testCase "unused-variable"
          "Warn only about variables that aren't used in any execution trace."
      , testCase "stack-trace-loop"
          "Don't show variables that only get defined in a loopy stack trace as unused."
      ]

-- | Compile a script and return the warnings
testCase :: FilePath -> TestName -> TestTree
testCase fileName msg = goldenVsString msg outFile $ do
    runCompile (compileFile inFile) >>= \case
      Left err -> panic $ toSL err
      Right CheckedScript{ checkedScriptWarnings = warnings }
        -> pure . toSL . prettyPrint $ warnings
  where
    inFile = inPath fileName
    outFile = outPath fileName
