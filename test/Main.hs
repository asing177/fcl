module Main where

import           Protolude
import           Test.Tasty
import           TestScript
import           TestUndefinedness
import           TestWarning


-------------------------------------------------------------------------------
-- test Suite
-------------------------------------------------------------------------------

suite :: TestTree
suite =
  testGroup "Test Suite"
  -- Undefinedness tests
  [ TestUndefinedness.undefinednessTests

  -- Script parser/pretty printer Tests
  , TestScript.scriptPropTests

  -- Script compilation and evaluation Tests
  , TestScript.scriptCompilerTests

  -- Script warning tests
  , TestWarning.warningTests

  -- Binary Serialization Tests
  -- , TestBinary.binaryTests
  ]

-------------------------------------------------------------------------------
-- Test Options
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Test Runner
-------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain suite
