{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
module Language.FCL.SafeWorkflow.REPL
  ( module Language.FCL.SafeWorkflow.REPL
  ) where

import Protolude hiding (Type, sequence, option)

import Data.List.List2 (List2(..))
import Data.Monoid (Dual(..))

import qualified Data.Map as M
import qualified Data.ByteString as BS

import Control.Monad.Gen
import Control.Monad.RWS hiding (sequence)

import System.FilePath (FilePath, (</>))
import System.Directory (createDirectoryIfMissing)

import Numeric.Lossless.Decimal (Decimal(..))

import Language.FCL.AST
import Language.FCL.Address (Address(..))
import Language.FCL.Graphviz (workflowWriteSVG)
import Language.FCL.SafeWorkflow.Editable
import Language.FCL.SafeWorkflow.CodeGen (CGInfo(..), codeGenScript, fromPreconds, fromArgs)
import Language.FCL.Pretty (prettyPrint)

import qualified Language.FCL.SafeWorkflow.Editable as Edit

-- TODO: currently Options has no impact on any operation, refactor
-- | Options for running the safe workflow REPL
data Options = Options
  { logDirectory   :: FilePath
  , loggingEnabled :: Bool
  } deriving (Eq, Ord, Show)

-- | Default options for running the safe workflow REPL
defaultOpts :: Options
defaultOpts = Options
  { logDirectory   = "./logs"
  , loggingEnabled = False
  }

-- TODO: extend history with global and method annotation edits
-- | Edit history for the safe workflow REPL.
type History = Dual [EditableSW]

-- | Safe workflow REPL (read eval print loop) monad
-- Keeps track of the safe workflow being edited,
-- and has logging capabilities too.
type SWREPLM = RWST Options History CGInfo (Gen TransId)

initInfo :: CGInfo
initInfo = CGInfo mempty mempty (Hole 1)

runSWREPLPure :: Options -> SWREPLM a -> (a, CGInfo, History)
runSWREPLPure opts actionM = runGen $ runRWST actionM opts initInfo

runSWREPLWithLogging :: SWREPLM () -> IO ()
runSWREPLWithLogging
  = void
  . runSWREPLIOWithOpts (defaultOpts {loggingEnabled = True})

runSWREPLIOWithOpts :: Options -> SWREPLM a -> IO (a, CGInfo, History)
runSWREPLIOWithOpts opts@Options{..} actionM = do
  let r@(_, _, Dual history) = runSWREPLPure opts actionM
      history' = Hole 1 : reverse history
  when loggingEnabled $ do
    createDirectoryIfMissing True logDirectory
    forM_ (zip history' [0..]) $ \(log, idx) -> do
      let filePath = logDirectory </> show idx
      printSW filePath log
  return r

-- TODO: log method and global changes as well
-- | Updates the stored workflow and logs the new result.
loggedModify :: (EditableSW -> EditableSW) -> SWREPLM ()
loggedModify transform = do
  s@CGInfo{..} <- get
  let sw' = transform editableWorkflow
  tell $ Dual [sw']
  put $ s { editableWorkflow = sw' }

noCode :: CGMetadata
noCode = CGMetadata Nothing Nothing

withoutCond :: Expr -> CGMetadata
withoutCond code = CGMetadata (Just code) Nothing

-- | Finish a hole, replace it with a simple transition.
finish
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Name         -- ^ Name of the new transition
  -> Expr         -- ^ Code for the transition
  -> SWREPLM ()
finish holeId atomName code = do
  transId <- gen
  let cont = Edit.Atom (TEL transId atomName False $ withoutCond code)
  initMethodAnnots atomName
  loggedModify (replaceHole holeId cont)

-- | Replace a hole with a parallel subworkflow.
parallel
  :: TransId                 -- ^ Identifier of hole to be replaced
  -> Name                   -- ^ Name of splitting transition
  -> Expr                   -- ^ Code for the splitting transitions
  -> Name                   -- ^ Name of joining transition
  -> Expr                   -- ^ Code for the joining transitions
  -> List2 (Name, Name)     -- ^ Names for the places inside the AND-branches
  -> SWREPLM ()
parallel holeId splitName splitCode joinName joinCode names = do
  splitId <- gen
  joinId  <- gen
  let splitLabel  = TEL splitId splitName False $ withoutCond splitCode
      joinLabel   = TEL joinId  joinName  False $ withoutCond joinCode
      numBranches = length names
  branchLabels <- forM names $ \(inName, outName) -> do
    inId  <- gen
    outId <- gen
    pure $ ANDBranchLabels (LPlace inName inId) (LPlace outName outId)
  let cont = Edit.AND splitLabel joinLabel branchLabels

  initMethodAnnots splitName
  initMethodAnnots joinName
  loggedModify (replaceHole holeId cont)

option
  :: TransId
  -> SWREPLM ()
option holeId = do
  loggedModify (replaceHole holeId Edit.UndetXOR)

-- TODO: only single condition then automatically negate it?
-- | Replace a hole with a branching subworkflow.
conditional
  :: TransId       -- ^ Identifier of hole to be replaced
  -- -> Name          -- ^ Name of the @then@ transition
  -- -> Name          -- ^ Name of the @else@ transition
  -> Expr          -- ^ The condition for the @then@ branch (this will be negated for the @else@ branch)
  -> SWREPLM ()
conditional holeId {- thenName elseName -} thenCond = do
  thenId <- gen
  elseId <- gen
  -- NOTE: since the transitions are editable, the names don't matter for rendering
  let thenLabel = TEL thenId "" True (CGMetadata Nothing $ Just thenCond)
      elseLabel = TEL elseId "" True (CGMetadata Nothing $ Just (neg thenCond))
  loggedModify (replaceHole holeId (Edit.IfXOR thenLabel elseLabel))

-- | Replace a hole with a "stay-or-continue" construct.
-- Stay in the current state or progress forward.
stayOrContinue
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Expr          -- ^ Condition to satisfy in order to continue
  -> SWREPLM ()
stayOrContinue holeId cond = do
  fallThroughId <- gen
  jumpBackId    <- gen
  let jumpBackLabel    = TEL jumpBackId    "" True (CGMetadata Nothing $ Just (neg cond))
      fallThroughLabel = TEL fallThroughId "" True (CGMetadata Nothing $ Just cond)
  loggedModify (replaceHole holeId $ Edit.SimpleLoop jumpBackLabel fallThroughLabel)

-- | Replace a hole with a "loop-or-continue" construct.
-- Loop in the the current state with the option to exit.
loopOrContinue
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Expr         -- ^ Condition to exit from the loop
  -> Name         -- ^ Name for the breakpoint
  -> SWREPLM ()
loopOrContinue holeId cond breakPointName = do
  breakPointId <- gen
  beforeId     <- gen
  afterId      <- gen
  jumpBackId   <- gen
  let breakPointLabel = LPlace breakPointName breakPointId
      beforeLabel     = TEL beforeId   "" True (CGMetadata Nothing Nothing)
      afterLabel      = TEL afterId    "" True (CGMetadata Nothing $ Just cond)
      jumpBackLabel   = TEL jumpBackId "" True (CGMetadata Nothing $ Just (neg cond))
  loggedModify (replaceHole holeId $ Edit.Loop breakPointLabel beforeLabel afterLabel jumpBackLabel)

-- | Replace a hole wth a sequence of two subworkflows.
sequence
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Name         -- ^ Name for the place in-between the the two subworklows
  -> SWREPLM ()
sequence holeId inbetweenName = do
  inbetweenId <- gen
  let cont = Edit.Seq (LPlace inbetweenName inbetweenId)
  loggedModify (replaceHole holeId cont)

-- TODO: See "ACF Continutation" note in Language.FCL.SafeWorkflow.Editable
acf
  :: TransId       -- ^ Identifier of hole to be replaced
  -> SWREPLM ()
acf = panic "not implemented"

initMethodAnnots
  :: Name
  -> SWREPLM ()
initMethodAnnots methodName = do
  s@CGInfo{..} <- get
  when (methodName `M.notMember` methodAnnotations) $
    put $ s { methodAnnotations = M.insert methodName mempty methodAnnotations }

-- TODO: add logging
addPrecondition
  :: Name
  -> Precondition
  -> Expr
  -> SWREPLM ()
addPrecondition methodName precondType precondExpr = do
  s@CGInfo{..} <- get
  let methodAnnots' = M.insertWith (<>) methodName
        (fromPreconds [(precondType, noLoc precondExpr)])
        methodAnnotations
  put $ s { methodAnnotations = methodAnnots' }

addRole
  :: Name
  -> Name
  -> SWREPLM ()
addRole methodName roleName =
  addPrecondition methodName PrecRoles (role roleName)

addArgs
  :: Name
  -> [Arg]
  -> SWREPLM ()
addArgs methodName args = do
  s@CGInfo{..} <- get
  let methodAnnots' = M.insertWith (<>) methodName
        (fromArgs args)
        methodAnnotations
  put $ s { methodAnnotations = methodAnnots' }

addGlobal
  :: Name
  -> Type
  -> Preconditions
  -> Maybe Expr
  -> SWREPLM ()
addGlobal name ty preconds mDefaultVal = do
  s@CGInfo{..} <- get
  let newGlobal = case mDefaultVal of
        Nothing     -> GlobalDefNull ty preconds (noLoc name)
        Just defVal -> GlobalDef     ty preconds name (noLoc defVal)
  put $ s { globalVariables = newGlobal : globalVariables }

addGlobalSimple
  :: Name
  -> Type
  -> SWREPLM ()
addGlobalSimple name ty = addGlobal name ty [] Nothing

addGlobalWithDefault
  :: Name
  -> Type
  -> Expr
  -> SWREPLM ()
addGlobalWithDefault name ty def = addGlobal name ty [] (Just def)

printSW :: FilePath -> EditableSW -> IO ()
printSW path = workflowWriteSVG path . prettify

execCodeGen :: SWREPLM a -> Script
execCodeGen = codeGenScript
            . snd3
            . runSWREPLPure defaultOpts
  where
    snd3 (_,x,_) = x

execCodeGenThenPrintScript :: SWREPLM a -> IO ()
execCodeGenThenPrintScript = putStr . prettyPrint . execCodeGen where

trueCond :: Expr
trueCond = ELit . noLoc . LBool $ True

zeroLTOne :: Expr
zeroLTOne = EBinOp (noLoc Lesser)
  (noLoc $ ELit $ noLoc $ LNum $ Decimal 0 0)
  (noLoc $ ELit $ noLoc $ LNum $ Decimal 0 1)

xLTFive :: Expr
xLTFive = EBinOp (noLoc Lesser)
  (noLoc $ EVar $ noLoc $ "x")
  (noLoc $ ELit $ noLoc $ LNum $ Decimal 0 5)

xEQZero :: Expr
xEQZero = EBinOp (noLoc Equal)
  (noLoc $ EVar $ noLoc $ "x")
  (noLoc $ ELit $ noLoc $ LNum $ Decimal 0 0)

xAssign :: Integer -> Expr
xAssign n = EAssign ["x"]
  (noLoc $ ELit $ noLoc $ LNum $ Decimal 0 n)

neg :: Expr -> Expr
neg = EUnOp (noLoc Not) . noLoc

role :: Name -> Expr
role = EVar . noLoc

mkArg :: Type -> Name -> Arg
mkArg ty name = Arg ty (noLoc name)

account :: Word8 -> Expr
account = ELit . noLoc . LAccount . Address . BS.singleton

decimal :: Integer -> Type
decimal = TNum . NPDecimalPlaces

simpleWhiteBoardExample3 :: SWREPLM ()
simpleWhiteBoardExample3 = do
  parallel 1
    "split" (xAssign 10)
    "join"  (xAssign 20)
    [ ("lhsIn", "lhsOut")
    , ("rhsIn", "rhsOut")
    ]
  finish 1 "t1" $ xAssign 1
  conditional 2 xLTFive
  finish 8 "t2" $ xAssign 2
  stayOrContinue 9 xEQZero
  finish 11 "t2" $ xAssign 3
  finish 12 "t2" $ xAssign 4

conditionalInConditionalLeft :: SWREPLM ()
conditionalInConditionalLeft = do
  conditional 1 xLTFive
  conditional 1 xEQZero
  finish 2 "t1" $ xAssign 1
  finish 3 "t1" $ xAssign 2
  finish 4 "t1" $ xAssign 3

conditionalInConditionalRight :: SWREPLM ()
conditionalInConditionalRight = do
  conditional 1 xLTFive
  finish 1 "t1" $ xAssign 1
  conditional 2 xEQZero
  finish 4 "t1" $ xAssign 2
  finish 5 "t1" $ xAssign 3

seqInConditionalLeft :: SWREPLM ()
seqInConditionalLeft = do
  conditional 1 xLTFive
  sequence 1 "inbetween"
  finish 11 "t1" $ xAssign 1
  finish 12 "t2" $ xAssign 2
  finish 2  "t1" $ xAssign 3

seqInConditionalRight :: SWREPLM ()
seqInConditionalRight = do
  conditional 1 xLTFive
  sequence 2 "inbetween"
  finish 21 "t1" $ xAssign 1
  finish 22 "t2" $ xAssign 2
  finish 1  "t1" $ xAssign 3

simpleOption :: SWREPLM ()
simpleOption = do
  addGlobalWithDefault "alice" TAccount (account 100)
  addGlobalWithDefault "bob"   TAccount (account 101)
  option 1
  finish 1 "t1" $ xAssign 1
  finish 2 "t2" $ xAssign 2
  addPrecondition "t1" PrecRoles (role "alice")
  addPrecondition "t2" PrecRoles (role "bob")
  addArgs "t1" [mkArg TBool "b"]

-- FIXME: you shouldn't be able to do undeterministic branching
-- inside a deterministic IF condition
-- TODO: disallow this
optionInConditionalLeft :: SWREPLM ()
optionInConditionalLeft = do
  conditional 1 xLTFive
  option 1
  finish 11 "t1" $ xAssign 1
  finish 12 "t2" $ xAssign 2
  finish 2  "t1" $ xAssign 3

loanContract :: SWREPLM ()
loanContract = do
  addGlobalSimple "principle"     $ decimal 2
  addGlobalSimple "interest_rate" $ decimal 2
  addGlobalSimple "currency"      $ TAsset $ decimal 2
  addGlobalSimple "borrower"        TAccount
  addGlobalSimple "lender"          TAccount
  addGlobalSimple "loan_contract"   TText

  sequence 1 "negotiate_terms"
  finish 1 "propose_contract" $ ENoOp
  loopOrContinue 2 ENoOp "make_decision"
  finish 4 "propose_terms" $ ENoOp
  finish 6 "revise" $ ENoOp
  option 5
  finish 2 "reject" $ ENoOp
  sequence 1 "signed"
  finish 1 "sign" $ ENoOp
  sequence 2 "contract_active"
  finish 1 "loan_start" $ ENoOp
  stayOrContinue 2 ENoOp
  finish 15 "pay_interest" $ ENoOp
  finish 14 "payback" $ ENoOp

  addRole "propose_terms" "lender"
  addRole "sign"          "borrower"
  addRole "revise"        "borrower"
  addRole "reject"        "borrower"
  addRole "loan_start"    "lender"
  addRole "loan_interest" "borrower"
  addRole "payback"       "borrower"
