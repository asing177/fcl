module Main (
  main,
) where

import Protolude

import Test.Tasty
import Test.Tasty.Hspec
import qualified Test.Key                  as Key
import qualified Test.Script               as Script
import qualified Test.Undefinedness        as Undefinedness
import qualified Test.Number               as Number
import qualified Test.Json                 as Json
import qualified Test.Storage              as Storage
import qualified Test.Workflow.Compilation as Workflow
import qualified Test.Workflow.Soundness   as Workflow
import qualified Test.Swagger              as Swagger
import qualified Test.Encoding             as Encoding

-------------------------------------------------------------------------------
-- test Suite
-------------------------------------------------------------------------------

suite :: IO TestTree
suite
  = do
  workflowTests      <- Workflow.compilationTests
  compilerTests      <- Script.compilerTests
  evalTests          <- Script.evalTests
  undefinednessTests <- Undefinedness.undefinednessTests
  pure $ testGroup "FCL tests" [

    -- Evaluator tests
    evalTests

    -- JSON Serialize Tests
    , Json.jsonTests

    -- Contract Storage Tests
    , Storage.storageTests

    -- Cryptography Tests
    , Key.keyTests

    -- Undefinedness tests
    , undefinednessTests

    -- Script parser/pretty printer Tests
    , Script.scriptPropTests

    -- Script compiler golden tests
    , compilerTests

    -- Workflow checker and graphviz output tests
    , workflowTests

    -- Non-lossy arithmetic tests
    , Number.numberTests

    -- Confirming the soundness checking algorithm's correctness on safely constructed workflows
    , Workflow.soundnessTests

    , Encoding.encodingTests
    ]

-------------------------------------------------------------------------------
-- Test Runner
-------------------------------------------------------------------------------

main :: IO ()
main = do
  swaggerTests <- testSpec "Swagger test" $ Swagger.swaggerTest
  fclTests <- suite
  defaultMain $ testGroup "All tests"
    [ fclTests
    , swaggerTests
    ]
