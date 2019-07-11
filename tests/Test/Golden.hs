{-# LANGUAGE FlexibleContexts #-}

module Test.Golden
  ( FileExtension
  , discoverGoldenTests
  , discoverGoldenTestsFCL
  , expectFailure
  , expectSuccess
  -- ** re-exports
  , TestTree
  , testGroup
  ) where

import Protolude

import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.String (String, lines, unlines)
import System.FilePath (replaceExtension)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced
import qualified System.IO.Strict

import Language.FCL.Pretty (Pretty, prettyPrint)

-- | Convert an IO action that given a 'FilePath' returns an @Either a b@ into
-- a continuation that returns a 'String', as required by 'discoverGoldenTests'.
-- The 'String' gets produced via the 'Pretty' constraint on @a@ and @b@.
-- 'expectSuccess' automatically fails on a 'Left' result and
-- 'expectSuccess' automatically fails on a 'Right' result.
expectFailure, expectSuccess
  :: (Pretty a, Pretty b)
  => (FilePath -> IO (Either a b)) -- ^ The IO action to run that will give either failure or success
  -> (FilePath -> IO String)
expectSuccess action = \file -> either (panic . toS . prettyPrint) (toS . prettyPrint) <$> action file
expectFailure action = \file -> either (toS . prettyPrint) (panic . toS . prettyPrint) <$> action file

type FileExtension = String

-- | Discover golden tests for input files with extension @.s@ or @.fcl@ and output
-- files with extension @.out@.
discoverGoldenTestsFCL
  :: FilePath -- ^ the directory in which to recursively look for golden tests
  -> (FilePath -> IO String) -- ^ the IO action to run on the input file which produces the test output
  -> IO TestTree
discoverGoldenTestsFCL = discoverGoldenTests [".s",".fcl"] ".out" (\fp s -> writeFile fp (toS s))

-- | Discover golden tests.
discoverGoldenTests
  :: [FileExtension] -- ^ the input file extensions
  -> FileExtension -- ^ the output file extension
  -> (FilePath -> String -> IO ()) -- ^ the IO action to run when creating (or updating, in the case of @--accept@) the golden file
  -> FilePath -- ^ the directory in which to recursively look for golden tests
  -> (FilePath -> IO String) -- ^ the IO action to run on the input file which produces the test output
  -> IO TestTree
discoverGoldenTests exts_in ext_out createGolden path mkOutput = pure
    . testGroup path
    . map (mkGoldenTest mkOutput ext_out createGolden)
    =<< findByExtension exts_in path

-- | Make a single golden test.
mkGoldenTest
  :: (FilePath -> IO String) -- ^ the action to test
  -> FileExtension -- ^ the extension of the outfile, e.g. @".out"@
  -> (FilePath -> String -> IO ())
  -> FilePath -- ^ the file path of the input file
  -> TestTree
mkGoldenTest mkOutput ext createGolden file = goldenTest
    file
    (System.IO.Strict.readFile outfile)
    (mkOutput file)
    checkDifference
    (createGolden outfile)
  where
    outfile = replaceExtension file ext
    checkDifference :: String -> String -> IO (Maybe String)
    checkDifference exp act = if exp == act
      then return Nothing
      else return . Just $ unlines
        [ "Contents of " <> outfile <> " (<) and actual output (>) differ:"
        , ppDiff $ getGroupedDiff (lines exp) (lines act)
      ]

