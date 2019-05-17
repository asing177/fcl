{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module TestScript
  ( evalTests
  , compilerTests
  , scriptPropTests
  ) where

import Protolude hiding (Type)
import Prelude (String)

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

import TestArbitrary ()
import Golden

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

instance Arbitrary Loc where
  arbitrary = Loc <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary

-- This is basically liftArbitrary from Arbitrary1
addLoc :: Gen a -> Gen (Located a)
addLoc g = Located <$> arbitrary <*> g

instance Arbitrary BinOp where
  arbitrary = elements
    [ Add
    , Sub
    , Mul
    , Div
    , And
    , Or
    , Equal
    , NEqual
    , LEqual
    , GEqual
    , Lesser
    , Greater
    ]

instance Arbitrary UnOp where
  arbitrary = pure Not

instance Arbitrary Lit where
  -- Missing literals:
  --  + LDateTime: missing instance Arbitrary DateTime (!)
  --  + LTimeDelta: missing instance Arbitrary TimeDelta (!)
  --  + LSig: not part of concrete syntax
  arbitrary = oneof
    [ LNum <$> arbitrary `suchThat` ((>= 0) . decimalPlaces)
      -- specifically, for 'LNum', ensure that we choose the 'NumDecimal' case
      -- and not 'NumRational', since there are no rational literals.
    , LBool     <$> arbitrary
    , LState    <$> arbitrary
    , LAccount  <$> arbitrary
    , LAsset    <$> arbitrary
    , LContract <$> arbitrary
    , LConstr   <$> arbitrary
    , pure LVoid
    ]

instance Arbitrary Type where
  arbitrary = oneof
    [ TNum <$> arbitrary
    , pure TBool
    , pure TAccount
    , TAsset <$> arbitrary
    , pure TContract
    , pure TVoid
    , TEnum <$> arbitrary
    ]

instance Arbitrary NumPrecision where
  arbitrary = oneof
    [ pure NPArbitrary
    , NPDecimalPlaces <$> arbitrary
    ]

instance Arbitrary Def where
  arbitrary = oneof
    [ GlobalDef <$> arbitrary <*> arbitrary <*> arbitrary <*> addLoc (sized arbNonSeqExpr)
    ]

instance Arbitrary Arg where
  arbitrary = Arg <$> arbitrary <*> arbitrary

instance Arbitrary Preconditions where
  arbitrary = Preconditions <$> arbitrary

instance Arbitrary Precondition where
  arbitrary = oneof [ pure PrecAfter, pure PrecBefore, pure PrecRoles ]

instance Arbitrary Method where
  arbitrary = Method <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> sized arbLExpr

instance Arbitrary Helper where
  arbitrary = Helper <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Transition where
  arbitrary = Arrow <$> arbitrary <*> arbitrary

instance Arbitrary EnumDef where
  arbitrary = EnumDef <$> arbitrary <*> listOf1 arbitrary

instance Arbitrary Script where
  arbitrary = Script <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Expr where
  arbitrary = sized arbNonSeqExpr

arbNumLogicExpr :: Int -> Gen Expr
arbNumLogicExpr n
  | n <= 0
    = oneof $ [EVar <$> arbitrary] ++
      map (fmap ELit . addLoc)
            [ LNum <$> arbitrary
            , LBool <$> arbitrary
            ]
  | otherwise = let n' = n `div` 2 in oneof
      [ EBinOp <$> arbitrary
               <*> addLoc (arbNumLogicExpr n')
               <*> addLoc (arbNumLogicExpr n')
      , EUnOp <$> arbitrary <*> addLoc (arbNumLogicExpr n')
      ]

arbMatches :: Int -> Gen [Match]
arbMatches n = listOf1 (Match <$> arbPat <*> arbLExpr n)

arbPat :: Gen LPattern
arbPat = Located <$> arbitrary <*> (PatLit <$> arbitrary)

instance Arbitrary Prim.PrimOp where
  arbitrary = oneof
    [ pure Prim.Verify
    , pure Prim.Sign
    , pure Prim.Block
    , pure Prim.Deployer
    , pure Prim.Sender
    , pure Prim.Created
    , pure Prim.Address
    , pure Prim.Validator
    , pure Prim.Sha256
    , pure Prim.AccountExists
    , pure Prim.AssetExists
    , pure Prim.ContractExists
    , pure Prim.Terminate
    , pure Prim.Now
    , pure Prim.TransitionTo
    , pure Prim.CurrentState
    , pure Prim.TxHash
    , pure Prim.ContractValueExists
    , pure Prim.ContractState
    , pure Prim.IsBusinessDayUK
    , pure Prim.NextBusinessDayUK
    , pure Prim.IsBusinessDayNYSE
    , pure Prim.NextBusinessDayNYSE
    , pure Prim.Between
    , pure Prim.ContractValue
    , pure Prim.Round
    , pure Prim.RoundUp
    , pure Prim.RoundDown
    , pure Prim.RoundRem
    , pure Prim.RoundUpRem
    , pure Prim.RoundDownRem
    , Prim.AssetPrimOp <$> arbAssetPrim
    , Prim.MapPrimOp <$> arbMapPrim
    ]
    where
      arbAssetPrim = elements
        [ Prim.HolderBalance
        , Prim.TransferTo
        , Prim.TransferFrom
        , Prim.CirculateSupply
        , Prim.TransferHoldings
        ]

      arbMapPrim = elements
        [ Prim.MapInsert
        , Prim.MapDelete
        , Prim.MapLookup
        ]

arbNonSeqExpr :: Int -> Gen Expr
arbNonSeqExpr n
  | n <= 0 = oneof
             [ EVar <$> arbitrary
             , ELit <$> arbitrary
             ]
  | otherwise = let n' = n `div` 2 in oneof
      [ EAssign <$> arbitrary         <*> addLoc (arbNonSeqExpr n')
      , ECall   <$> arbitrary         <*> listOf (addLoc (arbNonSeqExpr n'))
      , EIf     <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n' <*> arbLExpr n'
      , EBefore <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'
      , EAfter  <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'
      , EBetween <$> addLoc (arbNonSeqExpr n')
                 <*> addLoc (arbNonSeqExpr n')
                 <*> arbLExpr n'
      , ECase <$> addLoc (arbNonSeqExpr n') <*> arbMatches n'
      , arbNumLogicExpr n
      ]

arbSeqExpr :: Int -> Gen Expr
arbSeqExpr n
  | n <= 0 = arbNonSeqExpr 0
  | otherwise = let n' = n `div` 2 in
      ESeq <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'

arbLExpr :: Int -> Gen LExpr
arbLExpr n = oneof . map addLoc $
  [ arbNonSeqExpr n, arbSeqExpr n ]

-------------------------------------------------------------------------------
-- Test trees
-------------------------------------------------------------------------------

parserRoundtripTest
  :: (Arbitrary a, Show a, Show err, Pretty.Pretty a, Eq a)
  => TestName
  -> (Text -> Either err a)
  -> TestTree
parserRoundtripTest propName parser
  = testProperty propName $ \inp ->
    case parser (Pretty.prettyPrint inp) of
      Left err   -> panic $ show err
      Right outp -> outp == inp

scriptPropTests :: TestTree
scriptPropTests
  = testGroup "Parser and Pretty Printer Tests"
    [ parserRoundtripTest "lit == parse (ppr lit)" Parser.parseLit
    , localOption (QuickCheckMaxSize 20) $
      parserRoundtripTest "expr == parse (ppr expr)" Parser.parseExpr
    , localOption (QuickCheckMaxSize 10) $
      parserRoundtripTest "script == parse (ppr script)" Parser.parseScript
    , testProperty "decimal == parse (ppr decimal)" $ \lit ->
        (case lit of LNum _ -> True; _ -> False) ==>
          case Parser.parseLit (Pretty.prettyPrint lit) of
            Left err   -> panic $ show err
            Right outp -> outp == lit
    ]

compilerTests :: IO TestTree
compilerTests = testGroup "script" <$> sequence
  [ discoverGoldenTests [".s"] "" (pure . pure . pure ()) "examples" (Golden.expectSuccess Compile.compileFile)
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
      Right bs <- Utils.safeRead file
      now <- Time.now
      let contractAddr = Ref.testAddr
      Right contract <-
        Init.createContract
          contractAddr
          Ref.testAddr
          (Just Ref.testTransactionCtx)
          Ref.testPriv
          now
          Ref.testAddr
          Ref.genesisWorld
          (toS bs)
      -- Right contract <- Init.createFauxContract
      --     Ref.testAddr
      --     (Just Ref.testTransactionCtx)
      --     Ref.testPriv
      --     now
      --     Ref.testAddr
      --     Ledger.genesisWorld
      --     script
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
