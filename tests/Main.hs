module Main (
  main,
) where

import Protolude

import Test.Tasty

import qualified KeyTests
import qualified TestScript
import qualified TestUndefinedness
import qualified TestNumber
import qualified TestJson
import qualified TestStorage

import qualified TestWorkflow

-------------------------------------------------------------------------------
-- test Suite
-------------------------------------------------------------------------------

suite :: IO TestTree
suite
  = do
  workflowTests <- TestWorkflow.workflowTests
  compilerTests <- TestScript.compilerTests
  evalTests <- TestScript.evalTests
  pure $ testGroup "Test Suite" [

    -- Evaluator tests
    evalTests

    -- JSON Serialize Tests
    , TestJson.jsonTests

    -- Contract Storage Tests
    , TestStorage.storageTests

    -- Cryptography Tests
    , KeyTests.keyTests

    -- Undefinedness tests
    , TestUndefinedness.undefinednessTests

    -- Script parser/pretty printer Tests
    , TestScript.scriptPropTests

    -- Script compiler golden tests
    , compilerTests

    -- Workflow checker and graphviz output tests
    , workflowTests

    -- Non-lossy arithmetic tests
    , TestNumber.numberTests
    ]

-------------------------------------------------------------------------------
-- Test Runner
-------------------------------------------------------------------------------

main :: IO ()
main = do
  tests <- suite
  defaultMain tests
