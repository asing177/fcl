{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module TestScript
  ( evalTests
  , compilerTests
  , scriptPropTests
  ) where

import Protolude hiding (Type)
import Prelude (String, error)

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Arrow ((&&&))
import Control.Monad (fail)

import qualified Data.Text as T
import Numeric.Lossless.Number (Decimal (..))
import System.FilePath (replaceExtension)

import qualified Language.FCL.Utils as Utils
import qualified Language.FCL.World
import qualified Language.FCL.Contract as Contract

import Language.FCL.AST
import Language.FCL.Storage
import qualified Language.FCL.Time as Time
import qualified Language.FCL.Eval as Eval
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Parser as Parser
import qualified Language.FCL.Compile as Compile
import qualified Language.FCL.Init as Init
import qualified Language.FCL.Prim as Prim
import qualified Language.FCL.Hash as Hash

import qualified Reference as Ref

import Golden

-------------------------------------------------------------------------------
-- Test trees
-------------------------------------------------------------------------------

parserRoundtripTest
  :: (Arbitrary a, Show a, Pretty.Pretty err, Pretty.Pretty a, Eq a)
  => TestName
  -> (Text -> Either err a)
  -> TestTree
parserRoundtripTest propName parser
  = testProperty propName $ \inp ->
    case parser (Pretty.prettyPrint inp) of
      Left err   -> error (toS . Pretty.prettyPrint $ Pretty.hsep [Pretty.ppr inp, "\n***************\n", Pretty.ppr err]) -- panic $ show err
      Right outp -> outp == inp

scriptPropTests :: TestTree
scriptPropTests
  = testGroup "Parser and Pretty Printer Tests"
    [ localOption (QuickCheckMaxSize 8) $
      parserRoundtripTest "lit == parse (ppr lit)" Parser.parseLit
    , localOption (QuickCheckMaxSize 8) $
      parserRoundtripTest "expr == parse (ppr expr)" Parser.parseExpr
    , localOption (QuickCheckMaxSize 4) $
      parserRoundtripTest "script == parse (ppr script)" Parser.parseScript
    , testProperty "decimal == parse (ppr decimal)" $ \lit ->
        (case lit of LNum _ -> True; _ -> False) ==>
          case Parser.parseLit (Pretty.prettyPrint lit) of
            Left err   -> panic $ show err
            Right outp -> outp == lit
    ]

compilerTests :: IO TestTree
compilerTests = testGroup "script" <$> sequence
  [ discoverGoldenTests [".s"] "" (\_ !_ -> pure ()) "examples" (Golden.expectSuccess Compile.compileFile)
    -- ↑ ensure files in `examples` directory compile
  , discoverGoldenTestsFCL "tests/script/positive" (Golden.expectSuccess Compile.compileFile)
    -- ↑ ensure these scripts compile and give the expected output
  , discoverGoldenTestsFCL "tests/script/negative" (Golden.expectFailure Compile.compileFile)
    -- ↑ ensure these scripts don't compile and give the expected error
  ]

-- | Run a script @X.s@ in a mock evaluation environment, calling methods of
-- that script as given by the @X.calls@ file and keep track of the deltas and
-- the final global storage. At the moment we are injecting a bunch of test
-- addresses into the script, where 'testAddr' corresponds to the address of the
-- transaction issuer.
evalTests :: IO TestTree
evalTests = testGroup "eval" <$> sequence
    [ discoverGoldenTestsFCL "tests/eval/positive" evalTestPositive
    , discoverGoldenTestsFCL "tests/eval/negative" evalTestNegative
    ]
  where
    evalTestPositive :: FilePath -> IO String
    evalTestPositive file = either
      Pretty.panicppr
      (\(_, resEvalState) -> toSL $ T.unlines
          [ "*** Deltas ***"
          , Utils.ppShow (Eval.deltas resEvalState)
          , ""
          , "*** Globals ***"
          , Utils.ppShow (Eval.globalStorage resEvalState)
          ])
      <$> evalTest file

    evalTestNegative :: FilePath -> IO String
    evalTestNegative file = either
      (toS . Pretty.prettyPrint)
      (panic . Utils.ppShow)
      <$> evalTest file

    evalTest :: FilePath -> IO (Either Eval.EvalFail (Contract.Contract, Eval.EvalState Ref.World))
    evalTest file = do
      script <- injectTestAddresses <$> Parser.parseFile file
      now <- Time.now
      let contractAddr = Ref.testAddr
      contractE <-
        Init.createContract
          contractAddr
          Ref.testAddr
          (Just Ref.testTransactionCtx)
          Ref.testPriv
          now
          Ref.testAddr
          Ref.genesisWorld
          (Pretty.prettyPrint script)
      case contractE of
        Left err -> panic err
        Right contract -> do
          evalCtx <- initTestEvalCtx (scriptHelpers script)
          calls <- parseCalls <$> readFile (replaceExtension file ".calls")
          let evalState = Eval.initEvalState contract Ref.genesisWorld
          foldM (evalMethod' evalCtx) (Right (contract, evalState)) calls

    evalMethod' _ (Left err) _ = pure (Left err)
    evalMethod' evalCtx (Right (contract, evalState)) (Right lname, args) =
        case Contract.lookupContractMethod (locVal lname) contract of
          Left err -> fail (show err ++ "\n" ++ show contract)
          Right method -> do
            eNewEvalState <- Eval.execEvalM
              evalCtx
              evalState
              (Eval.evalMethod method =<< mapM Eval.evalLExpr args)
            pure ((updateContract contract &&& identity) <$> eNewEvalState)

    updateContract contract evalState = contract
      { Contract.globalStorage = GlobalStorage (Eval.globalStorage evalState)
      , Contract.state         = Eval.workflowState evalState
      }

    -- | To pretty print this apply
    -- @T.unlines . map (Pretty.prettyPrint . uncurry ECall)@
    parseCalls :: Text -> [(Either Prim.PrimOp LName, [LExpr])]
    parseCalls
      = map (either Pretty.panicppr identity . Parser.parseCall)
      . T.lines

    injectTestAddresses :: Script -> Script
    injectTestAddresses s = s{ scriptDefs = testAddresses <> scriptDefs s }
      where
        testAddresses = map (\(nm, val) -> GlobalDef TAccount mempty (Name nm) (Located NoLoc $ ELit $ Located NoLoc $ val))
            [ ("testAddr", LAccount Ref.testAddr)
            , ("testAddr2", LAccount Ref.testAddr2)
            , ("testAddr3", LAccount Ref.testAddr3)
            ]
    initTestEvalCtx :: [Helper] -> IO Eval.EvalCtx
    initTestEvalCtx helpers = do
      now <- Time.now
      pure Eval.EvalCtx
        { Eval.currentTxCtx = Just Eval.TransactionCtx
            { Eval.transactionBlockIdx = 0
            , Eval.transactionHash = Hash.toHash ("DummyTx" :: ByteString)
-- = Hash.toHash (Ref.testTx Ref.testCall)
            , Eval.transactionBlockTs = now
            , Eval.transactionIssuer = Ref.testAddr
            }
        , Eval.currentValidator = Ref.testAddr
        , Eval.currentCreated = now
        , Eval.currentDeployer = Ref.testAddr
        , Eval.currentAddress = Ref.testAddr
        , Eval.currentPrivKey = Ref.testPriv
        , Eval.currentHelpers = helpers
        }
