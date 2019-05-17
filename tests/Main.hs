module Main (
  main,
) where

import Protolude

import Test.Tasty

import qualified KeyTests
-- import qualified TestBinary
-- import qualified TestJson
import qualified TestScript
import qualified TestUndefinedness
import qualified TestNumber

import qualified TestWorkflow

-------------------------------------------------------------------------------
-- test Suite
-------------------------------------------------------------------------------

suite :: IO TestTree
suite
  = do
  workflowTests <- TestWorkflow.workflowTests
  compilerTests <- TestScript.compilerTests
  pure $ testGroup "Test Suite" [

    -- Evaluator tests
    -- , TestScript.evalTests

    -- Cryptography Tests
    KeyTests.keyTests

    -- JSON Serialize Tests
    -- , TestJson.jsonTests

    -- Undefinedness tests
    , TestUndefinedness.undefinednessTests

    -- Script parser/pretty printer Tests
    , TestScript.scriptPropTests

    -- Script compiler golden tests
    , compilerTests

    -- Workflow checker and graphviz output tests
    , workflowTests
    -- Binary Serialization Tests
    -- , TestBinary.binaryTests

    -- Contract Storage Tests
    -- , TestStorage.storageTests

    -- Transaction Serialization Tests
    -- , TestTx.cerealTests

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
