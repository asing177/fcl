{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Test.Script
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
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Map as Map
import qualified Data.Text as T
import Numeric.Lossless.Number (Decimal (..))
import System.FilePath (replaceExtension)

import qualified Language.FCL.Utils as Utils
import qualified Language.FCL.World
import qualified Language.FCL.Contract as Contract

import Language.FCL.AST
import Language.FCL.Address
import Language.FCL.Storage
import qualified Language.FCL.Asset as Asset
import qualified Language.FCL.Time as Time
import qualified Language.FCL.Eval as Eval
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Parser as Parser
import qualified Language.FCL.Compile as Compile
import qualified Language.FCL.Init as Init
import qualified Language.FCL.Prim as Prim
import qualified Language.FCL.Hash as Hash

import qualified Test.Reference as Ref

import Test.Golden as Golden

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
    , localOption (QuickCheckTests  10000) $
      localOption (QuickCheckMaxSize 8) $
      parserRoundtripTest "expr == parse (ppr expr)" Parser.parseExpr
    , localOption (QuickCheckTests 10000) $
      localOption (QuickCheckMaxSize 4) $
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
      let world = updateWorldWithGlobals now (scriptDefs script)
      contractE <-
        Init.createContract
          contractAddr
          Ref.testAddr
          (Just Ref.testTransactionCtx)
          Ref.testPriv
          now
          Ref.testAddr
          world
          (Pretty.prettyPrint script)
      case contractE of
        Left err -> panic err
        Right contract -> do
          evalCtx <- initTestEvalCtx script
          calls <- parseCalls <$> readFile (replaceExtension file ".calls")
          let evalState = Eval.initEvalState contract world
          foldM (evalMethod' evalCtx) (Right (contract, evalState)) calls

    evalMethod' _ (Left err) _ = pure (Left err)
    evalMethod' evalCtx (Right (contract, evalState)) (Right lname, args) =
        case Contract.lookupContractMethod (locVal lname) contract of
          Nothing -> fail ("Unknown method " <> (toS . unName . locVal) lname <> "\n" <> show contract)
          Just method -> do
            eNewEvalState <- Eval.execEvalM
              evalCtx
              evalState
              (Eval.evalMethod method =<< mapM Eval.evalLExpr args)
            pure ((updateContract contract &&& identity) <$> eNewEvalState)

    updateContract contract evalState = contract
      { Contract.globalStorage = GlobalStorage (Eval.globalStorage evalState)
      , Contract.state         = Eval.workflowState evalState
      }

    -- To pretty print this apply
    -- @T.unlines . map (Pretty.prettyPrint . uncurry ECall)@
    parseCalls :: Text -> [(Either Prim.PrimOp LName, [LExpr])]
    parseCalls
      = map (either Pretty.panicppr identity . Parser.parseCall)
      . T.lines

    -- Update the world state with the top-level definitions
    updateWorldWithGlobals :: Time.Timestamp -> [Def] -> Ref.World
    updateWorldWithGlobals now defs = foldl' step Ref.genesisWorld defs
      where
        step :: Ref.World -> Def -> Ref.World
        step w = \case
          GlobalDef TAccount _ (Name nm) (Located _ (ELit (Located _ (LAccount addr))))
            -> w { Ref.accounts = Map.insert addr (Ref.Account (Ref.testPub) addr "America/New_York" mempty) (Ref.accounts w) }
          GlobalDef (TAsset (TNum (NPDecimalPlaces n))) _ (Name nm) (Located _ (ELit (Located _ (LAsset addr))))
            -> w { Ref.assets = addAsset (Ref.assets w)}
              where
                addAsset = Map.insert addr
                    $ Ref.Asset
                      nm
                      Ref.testAddr
                      now
                      initialBalance
                      shareHoldings
                      (getRef nm)
                      (Asset.Fractional n)
                      addr
                      mempty
          _ -> w

        getRef :: Text -> Maybe Ref.Ref
        getRef nm = case nm of
                      "usd" -> pure Ref.USD
                      "gbp" -> pure Ref.GBP
                      "eur" -> pure Ref.EUR
                      "chf" -> pure Ref.CHF
                      "token" -> pure Ref.Token
                      "security" -> pure Ref.Security
                      _ -> Nothing

        getAllAccounts :: [Address AAccount]
        getAllAccounts = foldl' (\acc def -> case def of
                                    GlobalDef TAccount _ _ (Located _ (ELit (Located _ (LAccount addr)))) -> addr : acc
                                    _ -> acc
                                ) mempty defs

        initialBalance :: Asset.Balance
        initialBalance = Asset.Balance 100000

        shareHoldings :: Ref.Holdings
        shareHoldings = Ref.Holdings (Map.fromList $ (\addr -> (Asset.AccountHolder addr, Asset.Balance 100)) <$> getAllAccounts)

    injectTestAddresses :: Script -> Script
    injectTestAddresses s = s{ scriptDefs = testAddresses <> scriptDefs s }
      where
        testAddresses = map (\(nm, val) -> GlobalDef TAccount mempty (Name nm) (Located NoLoc $ ELit $ Located NoLoc $ val))
            [ ("testAddr", LAccount Ref.testAddr)
            , ("testAddr2", LAccount Ref.testAddr2)
            , ("testAddr3", LAccount Ref.testAddr3)
            ]

    initTestEvalCtx :: Script -> IO Eval.EvalCtx
    initTestEvalCtx script = do
      -- Use a deterministic timestamp
      let now = 1562574113981591
      pure Eval.EvalCtx
        { Eval.currentTxCtx = Just Eval.TransactionCtx
            { Eval.transactionBlockIdx = 0
            , Eval.transactionHash = Hash.toHash ("DummyTx" :: ByteString)
            , Eval.transactionBlockTs = now
            , Eval.transactionIssuer = Ref.testAddr
            }
        , Eval.currentValidator = Ref.testAddr
        , Eval.currentCreated = now
        , Eval.currentDeployer = Ref.testAddr
        , Eval.currentAddress = Ref.testAddr
        , Eval.currentPrivKey = Ref.testPriv
        , Eval.currentHelpers = scriptHelpers script
        , Eval.currentConstructorFields = Eval.scriptConstructors script
        }
