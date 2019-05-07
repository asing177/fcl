{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestScript (
  scriptCompilerTests,
  scriptPropTests,
) where

import qualified Data.ByteString.Lazy.Internal
import           Prelude                       (String)
import           Protolude                     hiding (Type)

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (fail)

import qualified Data.ByteString               as BS
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T


import qualified Contract
import qualified Encoding
import qualified Fixed
import qualified Hash                          (sha256Raw)
import qualified Key
import qualified Ledger
import qualified Utils

import           SafeInteger
import qualified SafeString                    as SafeString
import           Script
import qualified Script.Compile                as Compile
import qualified Script.Eval                   as Eval
import qualified Script.Init                   as Init
import qualified Script.Parser                 as Parser
import qualified Script.Pretty                 as Pretty
import qualified Script.Prim                   as Prim
import qualified Script.Typecheck              as Typecheck
import           Storage
import qualified Time

import qualified Hash

import           Reference                     (AC, AS, C)
import qualified Reference                     as Ref
import           TestArbitrary

-- Note: For some reason, all the eval.out files for golden tests are found in
-- the directory `tests/golden/typecheck` instead of `tests/golden/eval`

scriptCompilerTests :: TestTree
scriptCompilerTests = testGroup "Script Compiler Tests"
  [ scriptGoldenTests
  , ensureExamplesCompileTests]

scriptsFolder :: FilePath
scriptsFolder = "test/scripts/"

goldenFolder :: FilePath
goldenFolder = "test/golden/"

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
  arbitrary = oneof . map pure $
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

instance (Arbitrary as, Arbitrary ac, Arbitrary c) => Arbitrary (Lit as ac c) where
  -- Missing literals:
  --  + LDateTime: missing instance Arbitrary DateTime (!)
  --  + LTimeDelta: missing instance Arbitrary TimeDelta (!)
  --  + LSig: not part of concrete syntax
  arbitrary = oneof
    [ LInt      <$> arbitrary
    , LFloat    <$> arbitrary
    , LFixed    <$> arbitrary
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
    [ pure TInt
    , pure TFloat
    , pure TBool
    , pure TAccount
    , pure (TAsset TDiscrete)
    , pure (TAsset TBinary)
    , TAsset . TFractional <$> arbitrary
    , pure TContract
    , pure TVoid
    , TEnum <$> arbitrary
    ]

instance (Arbitrary as, Arbitrary ac, Arbitrary c) => Arbitrary (Def as ac c) where
  arbitrary = oneof
    [ GlobalDef <$> arbitrary <*> arbitrary <*> arbitrary <*> addLoc (sized arbNonSeqExpr)
    ]

instance Arbitrary Arg where
  arbitrary = Arg <$> arbitrary <*> arbitrary

instance (Arbitrary as, Arbitrary ac, Arbitrary c) => Arbitrary (Preconditions as ac c) where
  arbitrary = Preconditions <$> arbitrary

instance Arbitrary Precondition where
  arbitrary = oneof [ pure PrecAfter, pure PrecBefore, pure PrecRoles ]

instance (Arbitrary as, Arbitrary ac, Arbitrary c) => Arbitrary (Method as ac c) where
  arbitrary = Method <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> sized arbLExpr

instance (Arbitrary as, Arbitrary ac, Arbitrary c) => Arbitrary (Helper as ac c) where
  arbitrary = Helper <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Transition where
  arbitrary = Arrow <$> arbitrary <*> arbitrary

instance Arbitrary EnumDef where
  arbitrary = EnumDef <$> arbitrary <*> listOf1 arbitrary

instance (Arbitrary as, Arbitrary ac, Arbitrary c) => Arbitrary (Script as ac c) where
  arbitrary = Script <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary as, Arbitrary ac, Arbitrary c) => Arbitrary (Expr as ac c) where
  arbitrary = sized arbNonSeqExpr

arbNumLogicExpr :: Int -> Gen (Expr as ac c)
arbNumLogicExpr n
  | n <= 0
    = oneof $ [EVar <$> arbitrary] ++
      map (fmap ELit . addLoc)
            [ LInt <$> arbitrary
            , LFloat <$> arbitrary
            , LFixed <$> arbitrary
            , LBool <$> arbitrary
            ]
  | otherwise = let n' = n `div` 2 in oneof
      [ EBinOp <$> arbitrary
               <*> addLoc (arbNumLogicExpr n')
               <*> addLoc (arbNumLogicExpr n')
      , EUnOp <$> arbitrary <*> addLoc (arbNumLogicExpr n')
      ]

arbMatches :: (Arbitrary as, Arbitrary ac, Arbitrary c) => Int -> Gen [Match as ac c]
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
    , pure Prim.Fixed1ToFloat
    , pure Prim.Fixed2ToFloat
    , pure Prim.Fixed3ToFloat
    , pure Prim.Fixed4ToFloat
    , pure Prim.Fixed5ToFloat
    , pure Prim.Fixed6ToFloat
    , pure Prim.FloatToFixed1
    , pure Prim.FloatToFixed2
    , pure Prim.FloatToFixed3
    , pure Prim.FloatToFixed4
    , pure Prim.FloatToFixed5
    , pure Prim.FloatToFixed6
    , pure Prim.ContractValue
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

arbNonSeqExpr :: (Arbitrary as, Arbitrary ac, Arbitrary c) => Int -> Gen (Expr as ac c)
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

arbSeqExpr :: (Arbitrary as, Arbitrary ac, Arbitrary c) => Int -> Gen (Expr as ac c)
arbSeqExpr n
  | n <= 0 = arbNonSeqExpr 0
  | otherwise = let n' = n `div` 2 in
      ESeq <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'

arbLExpr :: (Arbitrary as, Arbitrary ac, Arbitrary c) => Int -> Gen (LExpr as ac c)
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
    [ parserRoundtripTest "lit == parse (ppr lit)" (Parser.parseLit Ref.customAddrParsers)
    , localOption (QuickCheckMaxSize 20) $
      parserRoundtripTest "expr == parse (ppr expr)" (Parser.parseExpr Ref.customAddrParsers)
    , localOption (QuickCheckMaxSize 15) $
      parserRoundtripTest "script == parse (ppr script)" (Parser.parseScript Ref.customAddrParsers)
    ]

runCompile :: ExceptT e (ReaderT Ref.CustomParsers IO) a -> IO (Either e a)
runCompile = (flip runReaderT) Ref.customAddrParsers . runExceptT


scriptGoldenTests :: TestTree
scriptGoldenTests = testGroup "Script Compiler Golden Tests"
    [ goldenVsStringDiff "Parser test for sample.s" differ parserOutFile $ do
        eSigs <- runCompile $ Compile.compileFile wellTypedFile
        case eSigs of
          Left err -> return $ toSL err
          Right Compile.CheckedScript{ Compile.checkedScript = ast }
            -> return $ toSL $ Utils.ppShow ast
    , goldenVsStringDiff "Signatures (typecheck) test for sample.s" differ sigsOutFile $ do
        eSigs <- runCompile $ Compile.compileFile wellTypedFile
        case eSigs of
          Left err -> return $ toSL err
          Right Compile.CheckedScript{ Compile.checkedScriptSigs = sigs }
            -> return $ toSL $ Utils.ppShow sigs
    , goldenVsStringDiff "Ill-type program test for sample_errs.s" differ errsOutFile $ do
        eSigs <- runCompile $ Compile.compileFile illTypedFile
        case eSigs of
          Left err -> return $ toSL err
          Right _  -> return "Type checking succeeded... this should not happen!"
    , evalTest "Eval outputs correct deltas and storage" evalFile evalOutFile [("f", [])]
    , evalTest "Eval outputs correct deltas and storage with top-level computation"
               (scriptsFolder <> "sample_eval_toplevel.s")
               (goldenFolder <> "typecheck/eval_toplevel.out")
               [("init", [])]
    , scriptTypesTests
    , scriptAnalysisGoldenTests
    , scriptEnumGoldenTests
    , scriptMapGoldenTests
    , scriptSetGoldenTests
    , scriptCollectionGoldenTests
    , scriptHelperGoldenTests
    , scriptDuplicateTests
    -- , scriptConcurrentTests
    , scriptParserGoldenTests
    , scriptHolesTests
    ]
  where
    wellTypedFile  = scriptsFolder <> "sample.s"
    illTypedFile   = scriptsFolder <> "sample_errs.s"
    evalFile       = scriptsFolder <> "sample_eval.s"

    notaryOutFile  = goldenFolder <> "typecheck/notary.out"
    minimalOutFile = goldenFolder <> "typecheck/minimal.out"
    graphOutFile   = goldenFolder <> "typecheck/graph.out"

    parserOutFile     = goldenFolder <> "typecheck/parser.out"
    sigsOutFile       = goldenFolder <> "typecheck/signatures.out"
    errsOutFile       = goldenFolder <> "typecheck/errors.out"
    evalOutFile       = goldenFolder <> "typecheck/eval.out"

differ :: (IsString a) => a -> a -> [a]
differ ref new = ["diff", "-u", ref, new]

-- | Given a Contract and a list of method names and arguments to each method,
-- accumulate the evalState while evaluation each method in order. Compares the
-- resulting contract storage output with the specified golden file, and
-- potentially the list of deltas.
evalTest
  :: TestName          -- ^ test name
  -> FilePath          -- ^ FCL file
  -> FilePath          -- ^ expected output
  -> [(Name, [Value AS AC C])] -- ^ Sequence of methods to call
  -> TestTree
evalTest testName inputFp outputFp funcsAndArgs = do
    goldenVsStringDiff testName differ outputFp $ do
      evalScript >>= \e -> do
        eResEvalState <- case e of
          Left err -> panic (show err)
          Right contract  -> runExceptT $ do
            let helpers = scriptHelpers (Contract.script contract)
                evalState = Eval.initEvalState contract Ref.genesisWorld
            evalCtx <- liftIO $ initTestEvalCtx helpers
            foldM (evalMethod' evalCtx) (contract, evalState) funcsAndArgs
        e' <- runExceptT $ case eResEvalState of
            Left err -> throwE $ Utils.ppShow err
            Right (_, resEvalState) -> do
              let globalStoreStr = Utils.ppShow (Eval.globalStorage resEvalState)
              return $ toSL $ T.intercalate "\n"
                    [ Utils.ppShow (Eval.deltas resEvalState), globalStoreStr ]
        case e' of
          Left err -> panic err
          Right x -> pure x
  where
    evalScript = (flip runReaderT) Ref.customAddrParsers . runExceptT $ do
        eBS <- liftIO $ Utils.safeRead inputFp
        case eBS of
          Left err -> throwE $ err
          Right bs -> do
            now <- liftIO $ Time.now
            contractAddr <- liftIO $ Ref.toContractAddr bs
            Init.createContract
                (Proxy :: Proxy (Ref.Asset, Ref.Account))
                contractAddr
                Ref.testAddr
                (Just Ref.testTransactionCtx)
                Ref.testPriv
                now
                Ref.testAddr
                Ref.genesisWorld
                (toS bs)


    evalMethod'
      :: Eval.EvalCtx AS AC C Ref.PrivateKey
      -> (Contract.Contract AS AC C, Eval.EvalState AS AC C Ref.Asset Ref.Account Ref.World)
      -> (Name, [Value AS AC C])
      -> ExceptT (Eval.EvalFail AS AC C) IO (Contract.Contract AS AC C, Eval.EvalState AS AC C Ref.Asset Ref.Account Ref.World)
    evalMethod' evalCtx (contract, evalState) (name, args) =
      case Contract.lookupContractMethod name contract of
            Left err     ->
              panic (toS $ show err ++ "\n" ++ show contract)
            Right method -> do
              eNewEvalState <-
                Eval.execEvalM evalCtx evalState (Eval.evalMethod method args)
              pure ((updateContract contract &&& identity) eNewEvalState)

    updateContract contract evalState = contract
      { Contract.globalStorage = Storage.GlobalStorage (Eval.globalStorage evalState)
      , Contract.state         = Eval.workflowState evalState
      }

scriptTypesTests
  = testGroup "Script type checker tests" $
      map (mkTypesTest negativeTest)
        [ ("terminate", "`terminate` primop is not of type `any`")
        , ("scoped-temp-bad", "temporary variables don't shadow within the same scope")
        , ("scoped-temp-bad-2", "temporary variables don't shadow in nested scopes")
        , ("invalid_prim_args", "incorrect number of arguments to prim op")
        , ("shadow-argument-method", "A method's argument is shadowed with a temporary variable")
        , ("shadow-argument-function", "A function's argument is shadowed with a temporary variable")
        , ("dead-code-1", "")
        , ("dead-code-2", "")
        , ("multiple-transitions", "")
        , ("no-transition", "")
        , ("sneaky-terminate", "")
        , ("shadow-helper-ass", "Helpers can't be shadowed by assignment")
        , ("shadow-global-helper-argument", "Disallow name clash between global and helper argument")
        , ("helper-name-clash", "Disallow two helpers of the same name")
        , ("sub-plus-mul-invalid", "Invalid additions/subtractions/multiplications must raise errors")
        ]
      <> map (mkTypesTest positiveTest)
        [ ("scoped-temp", "temporary variables don't leak out of their scope")
        , ("scoped-temp-2", "temporary variables don't leak out of their helper's scope")
        ]

  where
    mkTypesTest testType (file, descr)
      = testType
          descr
          (scriptsFolder <> "typecheck/" <> file <> ".s")
          (goldenFolder <> "typecheck/" <> file <> ".out")

scriptAnalysisGoldenTests :: TestTree
scriptAnalysisGoldenTests = testGroup "Script analysis golden tests"
                            [ undefinednessTests
                            , effectTests
                            , roleSystemTests
                            ]
  where
    undefinednessTests
      = testGroup "Undefinedness checks"
        (map positiveUndefinednessTest
              [ "loop"
              , "if_3"
              , "local"
              , "access_restriction_ok"
              ]
        ++ map negativeUndefinednessTest
              [ "different_paths"
              , "global"
              , "if"
              , "if_2"
              , "if_tmp"
              , "states"
              , "toplevel"
              , "access_restriction_bad"
              , "access_restriction_bad_2"
              , "collections"
              , "collections_2"
              ]
         )

    positiveUndefinednessTest testName
      = positiveTest ("Undefinedness check for " <> testName <> ".s")
                     (scriptsFolder <> "undefinedness/" <> testName <> ".s")
                     (goldenFolder <> "typecheck/undefinedness/" <> testName <> ".out")

    negativeUndefinednessTest testName
      = negativeTest ("Undefinedness check for " <> testName <> ".s")
                     (scriptsFolder <> "undefinedness/" <> testName <> ".s")
                     (goldenFolder <> "typecheck/undefinedness/" <> testName <> ".out")

    effectTests
      = testGroup "Effect analysis golden tests"
        [ negativeTest "Effect analysis for toplevel_side_effect.s"
                       (scriptsFolder <> "effects/toplevel_side_effect.s")
                       (goldenFolder <> "typecheck/effects/toplevel_side_effect.out")
        , negativeTest "Naive static check on temporal preconditions on variables"
                       (scriptsFolder <> "effects/temporal-preconditions.s")
                       (goldenFolder <> "typecheck/effects/temporal-preconditions.s")
        ]

    roleSystemTests
      = testGroup "Role system golden tests"
        [ positiveTest "Account literal in method access control (roles_ok.s)"
                       (scriptsFolder <> "roles/roles_ok.s")
                       (goldenFolder <> "roles/roles_ok.out")
        , negativeTest "Access restriction in global def is not well typed (set)."
               (scriptsFolder <> "roles/illtyped.s")
               (goldenFolder <> "roles/illtyped.out")
        , negativeTest "Access restriction in global def is not well typed (singleton)."
               (scriptsFolder <> "roles/illtyped2.s")
               (goldenFolder <> "roles/illtyped2.out")
        , evalTest
            "Eval doesn't proceed when issuer is not authorised" -- negative
            (scriptsFolder <> "roles/roles_ok.s")
            (goldenFolder <> "roles/role_not_authorised.out")
            [("init", [])]

        , evalTest
            "Eval proceeds when issuer is authorised" -- positive
            (scriptsFolder <> "roles/roles_test_addr.s")
            (goldenFolder <> "roles/role_authorised.out")
            [("init", [])]

        , negativeTest
            "Eval doesn't proceed when issuer is not authorised to edit var"
            (scriptsFolder <> "roles/roles_var_not_authorised.s")
            (goldenFolder <> "roles/role_var_not_authorised.out")
        , positiveTest
            "Eval proceeds when issuer is authorised to edit var"
            (scriptsFolder <> "roles/roles_var_ok.s")
            (goldenFolder <> "roles/role_var_ok.out")
        ]

scriptEnumGoldenTests :: TestTree
scriptEnumGoldenTests = testGroup "Script enum golden tests"
    [ negativeTest "Duplicate constructor test for double_constrs.s"
                   (scriptsFolder <> "enum/double_constrs.s")
                   (goldenFolder <> "typecheck/enum/double_constrs.out")
    , negativeTest "Duplicate enum definition test for double_def.s"
                   (scriptsFolder <> "enum/double_def.s")
                   (goldenFolder <> "typecheck/enum/double_def.out")
    , negativeTest "Empty enum definition test for empty_def.s"
                   (scriptsFolder <> "enum/empty_def.s")
                   (goldenFolder <> "typecheck/enum/empty_def.out")
    , negativeTest "Empty case statement test for empty_match.s"
                   (scriptsFolder <> "enum/empty_match.s")
                   (goldenFolder <> "typecheck/enum/empty_match.out")
    , positiveTest "Type checking test for enum.s"
                   (scriptsFolder <> "enum/enum.s")
                   (goldenFolder <> "typecheck/enum/enum.out")
    , positiveTest "Graph analysis test for graph.s"
                   (scriptsFolder <> "enum/graph.s")
                   (goldenFolder <> "typecheck/enum/graph.out")
    , negativeTest "Incomplete case statement test for incomplete_match.s"
                   (scriptsFolder <> "enum/incomplete_match.s")
                   (goldenFolder <> "typecheck/enum/incomplete_match.out")
    , negativeTest "Overlapping constructors between enums test for overlap_constrs.s"
                   (scriptsFolder <> "enum/overlap_constrs.s")
                   (goldenFolder <> "typecheck/enum/overlap_constrs.out")
    , negativeTest "Overlapping patterns test for overlap_match.s"
                   (scriptsFolder <> "enum/overlap_match.s")
                   (goldenFolder <> "typecheck/enum/overlap_match.out")
    , positiveTest "Undefinedness analysis test for uninitialized.s"
                   (scriptsFolder <> "enum/uninitialized.s")
                   (goldenFolder <> "typecheck/enum/uninitialized.out")
    , negativeTest "Undefined constructor test for unknown_constr.s"
                   (scriptsFolder <> "enum/unknown_constr.s")
                   (goldenFolder <> "typecheck/enum/unknown_constr.out")
    , negativeTest "Undefined constructor test for unknown_enum.s"
                   (scriptsFolder <> "enum/unknown_enum.s")
                   (goldenFolder <> "typecheck/enum/unknown_enum.out")
    ]

scriptMapGoldenTests :: TestTree
scriptMapGoldenTests = testGroup "Script map golden tests"
  [ negativeTest "Invalid types test for invalid_types.s"
                 (scriptsFolder <> "maps/invalid_types.s")
                 (goldenFolder <> "typecheck/maps/invalid_types.out")
  , evalTest "Evaluation of inserts, deletes, and modifies in maps"
             (scriptsFolder <> "maps/eval.s")
             (goldenFolder <> "typecheck/maps/eval.out")
             [("insertInvestor", [VAccount Ref.testAddr,  VEnum (EnumConstr "BigInvestor")])
             ,("insertInvestor", [VAccount Ref.testAddr2, VEnum (EnumConstr "SmallInvestor")])
             ,("insertInvestor", [VAccount Ref.testAddr3, VEnum (EnumConstr "BigInvestor")])
             ,("deleteInvestor", [VAccount Ref.testAddr3])
             ,("lookupInvestor", [VAccount Ref.testAddr])
             ,("rem100Shares",   [VAccount Ref.testAddr])
             ]
  , positiveTest "Asset lookup from map asset_lookup.s"
                 (scriptsFolder <> "maps/asset_lookup.s")
                 (goldenFolder <> "typecheck/maps/asset_lookup.out")
  , negativeTest "Invalid asset lookup from map asset_lookup_incorrect.s"
                 (scriptsFolder <> "maps/asset_lookup_incorrect.s")
                 (goldenFolder <> "typecheck/maps/asset_lookup_incorrect.out")
  , scriptMapTypecheckerBugTest
  ]

scriptMapTypecheckerBugTest :: TestTree
scriptMapTypecheckerBugTest =
  negativeTest "Insert invalid asset type into map from map lookup with different asset type"
               (scriptsFolder <> "maps/asset_lookup_insert_invalid.s")
               (goldenFolder <> "typecheck/maps/asset_lookup_insert_invalid.out")

scriptSetGoldenTests :: TestTree
scriptSetGoldenTests = testGroup "Script set golden tests"
  [ negativeTest "Invalid types test for invalid_types.s"
                 (scriptsFolder <> "sets/invalid_types.s")
                 (goldenFolder <> "typecheck/sets/invalid_types.out")
  , evalTest "Evaluation of inserts, and deletes in sets"
             (scriptsFolder <> "sets/eval.s")
             (goldenFolder <> "typecheck/sets/eval.out")
             [("insertInvestor", [VAccount Ref.testAddr,  VEnum (EnumConstr "BigInvestor")])
             ,("insertInvestor", [VAccount Ref.testAddr2, VEnum (EnumConstr "SmallInvestor")])
             ,("insertInvestor", [VAccount Ref.testAddr3, VEnum (EnumConstr "MedInvestor")])
             ,("deleteInvestor", [VAccount Ref.testAddr2, VEnum (EnumConstr "SmallInvestor")])
             ,("deleteInvestor", [VAccount Ref.testAddr,  VEnum (EnumConstr "BigInvestor")])
             ,("end", [])
             ]
  ]

scriptCollectionGoldenTests :: TestTree
scriptCollectionGoldenTests =
  testGroup "Script Collection type golden tests"
    [ negativeTest "Test expected typechecker failure for 'aggregate' primop using 'invalid_aggregate.s'"
                   (scriptsFolder <> "collections/invalid_aggregate.s")
                   (goldenFolder <> "typecheck/collections/invalid_aggregate.out")
    , evalTest "Test eval of higher-order collection prim op 'aggregate' using aggregate.s"
               (scriptsFolder <> "collections/aggregate.s")
               (goldenFolder <> "typecheck/collections/aggregate.out")
               [("sumBalances", [])]

    , negativeTest "Test expected typechecker failure for 'transform' primop using 'invalid_transform.s'"
                   (scriptsFolder <> "collections/invalid_transform.s")
                   (goldenFolder <> "typecheck/collections/invalid_transform.out")
    , evalTest "Test eval of higher-order collection prim op 'transform' using transform.s"
               (scriptsFolder <> "collections/transform.s")
               (goldenFolder <> "typecheck/collections/transform.out")
               [("applyInterest",[])]

    , negativeTest "Test expected typechecker failure for 'filter' primop using 'invalid_filter.s'"
                   (scriptsFolder <> "collections/invalid_filter.s")
                   (goldenFolder <> "typecheck/collections/invalid_filter.out")
    , evalTest "Test eval of higher-order collection prim op 'filter' using filter.s"
               (scriptsFolder <> "collections/filter.s")
               (goldenFolder <> "typecheck/collections/filter.out")
               [("filterTest",[])]
    , negativeTest "Test expected typechecker failure for 'element' primop using 'invalid_element.s'"
                   (scriptsFolder <> "collections/invalid_element.s")
                   (goldenFolder <> "typecheck/collections/invalid_element.out")
    , evalTest "Test eval of higher-order collection prim op 'element' using element.s"
               (scriptsFolder <> "collections/element.s")
               (goldenFolder <> "typecheck/collections/element.out")
               [("elementTest",[VAccount Ref.testAddr, VInt 500])]
    , negativeTest "Raise type error when initialising collections with wrong expressions"
                   (scriptsFolder <> "collections/expr_type_mismatches.s")
                   (goldenFolder <> "typecheck/collections/expr_type_mismatches.out")
    ]

scriptHelperGoldenTests :: TestTree
scriptHelperGoldenTests =
  testGroup "Script helper functions golden tests"
    [ negativeTest "Self recursion should be disallowed with invalid function name"
                   (scriptsFolder <> "helpers/simple_recursion.s")
                   (goldenFolder <> "typecheck/helpers/simple_recursion.out")
    , negativeTest "Mutual recursion should be disallowed with invalid function name"
                   (scriptsFolder <> "helpers/mutual_recursion.s")
                   (goldenFolder <> "typecheck/helpers/mutual_recursion.out")
    , negativeTest "PrimOp calls and global variable assignments disallowed in helper functions"
                   (scriptsFolder <> "helpers/disallow_effects.s")
                   (goldenFolder <> "typecheck/helpers/disallow_effects.out")
    , negativeTest "Helpers with invalid argument and return should not pass the typechecker"
                   (scriptsFolder <> "helpers/invalid_types.s")
                   (goldenFolder <> "typecheck/helpers/invalid_types.out")
    , evalTest "Helper function can use previously defined helper function"
               (scriptsFolder <> "helpers/helper_using_helper.s")
               (goldenFolder <> "typecheck/helpers/helper_using_helper.out")
               [("flipeeFlopee", [VEnum (EnumConstr "Flop")])
               ,("setX", [VInt 42])
               ]
    , evalTest "Example use of helper functions succeeds"
               (scriptsFolder <> "helpers/helpers.s")
               (goldenFolder <> "typecheck/helpers/helpers.out")
               [("f", [ VAccount (Ref.fromBS "43WRxMNcnYgZFcE36iohqrXKQdajUdAxeSn9mzE1ZedB")
                      , VFixed (Fixed.mkFixed Fixed.Prec2 5000)
                      ]
                )
               ]
    ]

scriptConcurrentTests :: TestTree
scriptConcurrentTests =
  testGroup "Concurrent workflow eval golden tests"
    [ evalTest "Concurrency test (interleaving 1)"
               (scriptsFolder <> "concurrency/concurrency_eval.s")
               (goldenFolder <> "typecheck/concurrency/concurrency_eval_1.out")
               [ ("f", [])
               , ("g", [])
               , ("h", [])
               , ("i", [])
               ]
    , evalTest "Concurrency test (interleaving 2)"
               (scriptsFolder <> "concurrency/concurrency_eval.s")
               (goldenFolder <> "typecheck/concurrency/concurrency_eval_2.out")
               [ ("f", [])
               , ("h", [])
               , ("g", [])
               , ("i", [])
               ]
    ]

-- TODO move all duplicate tests here
scriptDuplicateTests :: TestTree
scriptDuplicateTests = testGroup "Script duplicate/variable shadowing tests"
    [ duplTest "shadowing"
        "Variable shadowing with method argument"
    , duplTest "dupl-states-in-method-body"
        "Duplicate place in state in method body"
    , duplTest "dupl-states-in-method-prec"
        "Duplicate place in state in method precondition"
    , duplTest "dupl-states-in-transition"
        "Duplicate place in state in transition declaration"
    , duplTest "method-preconditions"
        "Duplicate precondition in method preconditions"
    , duplTest "method-preconditions-roles"
        "Duplicate precondition in method preconditions"
    , duplTest "transition"
        "Duplicate transition"
    ]
  where
    duplTest file descr = negativeTest
        descr
        (scriptsFolder <> "duplicates/" <> file <> ".s")
        (goldenFolder <> "duplicates/" <> file <> ".out")


-- | If compilation succeeds, the test succeeds
positiveTest :: TestName -> FilePath -> FilePath -> TestTree
positiveTest msg inFile outFile
  = goldenVsStringDiff msg differ outFile $ do
    eSigs <- runCompile $ Compile.compileFile inFile
    case eSigs of
      Left err -> panic $ toSL err
      Right script
        -> return $ toSL $ Pretty.prettyPrint script

-- | If compilation fails, the test succeeds
negativeTest :: TestName -> FilePath -> FilePath -> TestTree
negativeTest msg inFile outFile
  = goldenVsStringDiff msg differ outFile $ do
    eSigs <- runCompile $ Compile.compileFile inFile
    case eSigs of
      Left err -> return $ toSL err
      Right _ -> panic "Script analysis succeeded... this should not happen!"

initTestEvalCtx :: [Helper AS AC C] -> IO (Eval.EvalCtx AS AC C Ref.PrivateKey)
initTestEvalCtx helpers = do
  now <- Time.now
  pure Eval.EvalCtx
    { Eval.currentTxCtx = Just Eval.TransactionCtx
        { Eval.transactionBlockIdx = 0
        , Eval.transactionHash = Hash.toHash (Ref.testTx Ref.testCall)
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

ensureExamplesCompileTests :: TestTree
ensureExamplesCompileTests = withResource
    (findByExtension [".s"] "examples") -- IO [FP]
    (const $ pure ())
    (\scripts -> checkExamples scripts)

checkExamples :: IO [FilePath] -> TestTree
checkExamples fs
  = testCase "Compile example files" $ do
      errs <- getErrors =<< fs
      case errs of
        []   -> pure ()
        errs -> assertFailure . T.unpack . T.unlines $ errs
  where
    getErrors :: [FilePath] -> IO [Text]
    getErrors = foldM accumulateErrorsAndWarnings []

    accumulateErrorsAndWarnings :: [Text] -> FilePath -> IO [Text]
    accumulateErrorsAndWarnings acc f = runCompile (Compile.compileFile f) >>= \case
      Left err -> pure $ heading f <> err : acc
      Right Compile.CheckedScript{ Compile.checkedScriptWarnings = [] }
        -> pure acc
      Right Compile.CheckedScript{ Compile.checkedScriptWarnings = warnings }
        -> pure $ heading f <> Pretty.prettyPrint warnings : acc
      where
        heading f = "\n" <> T.pack f <> "\n" <> T.replicate (length f) "=" <> "\n"

scriptParserGoldenTests :: TestTree
scriptParserGoldenTests = testGroup "Parser golden tests" $
  map mkParserNegTest
    [ ("after-seq", "Ensure that sequencing in `after` guard expressions doesn't parse")
    , ("before-seq", "Ensure that sequencing in `before` guard expressions doesn't parse")
    , ("case-seq", "Ensure that sequencing in `case` scrutinee expressions doesn't parse")
    , ("if-seq", "Ensure that sequencing in `if` guard expressions doesn't parse")
    ]
  where
    mkParserNegTest (file, descr)
      = negativeTest
          descr
          (scriptsFolder <> "parse/negative/" <> file <> ".s")
          (goldenFolder <> "parse/negative/" <> file <> ".out")

scriptHolesTests :: TestTree
scriptHolesTests = testGroup "Holes tests" $
  map mkHolesTest
    [ ("holes", "report in-scope variables when hitting a hole")
    , ("collection", "work reasonably well with collections")
    , ("helper", "report in-scope variables in the context of helpers")
    ]
  where
    mkHolesTest (file, descr)
      = negativeTest
          descr
          (scriptsFolder <> "holes/" <> file <> ".s")
          (goldenFolder <> "holes/" <> file <> ".out")
