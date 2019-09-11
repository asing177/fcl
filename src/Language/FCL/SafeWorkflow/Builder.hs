{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
module Language.FCL.SafeWorkflow.Builder
  ( module Language.FCL.SafeWorkflow.Builder
  ) where

import Protolude hiding (Type, sequence, option)

import qualified GHC.Exts as GHC (IsList(..))

import Data.List.List2 (List2(..))
import Data.Monoid (Dual(..))

import qualified Data.Map as M

import Control.Monad.Gen
import Control.Monad.RWS hiding (sequence)

import System.FilePath (FilePath, (</>))
import System.Directory (createDirectoryIfMissing)

import Language.FCL.SafeWorkflow hiding
  ( Atom
  , AND
  , pattern SimpleLoop
  , pattern Loop
  , pattern XOR
  , pattern Seq
  , pattern ACF
  , PlaceId
  )
import Language.FCL.AST
import Language.FCL.Graphviz (workflowWriteSVG)
import Language.FCL.SafeWorkflow.Editable
import Language.FCL.SafeWorkflow.CodeGen (CGInfo(..), codeGenScript, fromPreconds, fromArgs, noLoc)
import Language.FCL.Pretty (prettyPrint)

import qualified Language.FCL.SafeWorkflow          as SW
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

-- | Safe workflow builder monad.
-- Keeps track of the safe workflow being edited,
-- and has logging capabilities too.
type Builder = RWST Options History CGInfo (Gen TransId)

putESW :: EditableSW -> Builder ()
putESW esw = do
  s@CGInfo{..} <- get
  put $ s { editableWorkflow = esw }

initInfo :: CGInfo
initInfo = CGInfo mempty mempty (Hole 1)

runBuilderPure :: Options -> Builder a -> (a, CGInfo, History)
runBuilderPure opts actionM = runGen $ runRWST actionM opts initInfo

runBuilderWithLogging :: Builder () -> IO ()
runBuilderWithLogging
  = void
  . runBuilderIOWithOpts (defaultOpts {loggingEnabled = True})

runBuilderIOWithOpts :: Options -> Builder a -> IO (a, CGInfo, History)
runBuilderIOWithOpts opts@Options{..} actionM = do
  let r@(_, _, Dual history) = runBuilderPure opts actionM
      history' = Hole 1 : reverse history
  when loggingEnabled $ do
    createDirectoryIfMissing True logDirectory
    forM_ (zip history' [0..]) $ \(log, idx) -> do
      let filePath = logDirectory </> show idx
      printSW filePath log
  return r

-- TODO: log method and global changes as well
-- | Updates the stored workflow and logs the new result.
logAction :: Builder a -> Builder ()
logAction action = do
  action
  CGInfo{..} <- get
  tell $ Dual [editableWorkflow]

printSW :: FilePath -> EditableSW -> IO ()
printSW path = workflowWriteSVG path . prettify

execCodeGen :: Builder a -> Script
execCodeGen = codeGenScript
            . snd3
            . runBuilderPure defaultOpts
  where
    snd3 (_,x,_) = x

execCodeGenThenPrintScript :: Builder a -> IO ()
execCodeGenThenPrintScript = putStr . prettyPrint . execCodeGen where

--------------------------
-- Low level primitives --
--------------------------

guardedBy :: TEditLabel -> Expr -> TEditLabel
-- guardedBy lbl _
--   | Just code <- cgmCode . trCGMetadata $ lbl
--   = panic $ "guardedBy: Label already contains code (no code displayed means NoOp): " <> prettyPrint code
guardedBy lbl@TEL{..} newCond
  | CGMetadata{..} <- trCGMetadata
  = case cgmIfCond of
    Nothing -> lbl { trCGMetadata = trCGMetadata { cgmIfCond = Just newCond } }
    Just origCond -> let combinedCond = EBinOp (noLoc And) (noLoc origCond) (noLoc newCond)
      in lbl { trCGMetadata = trCGMetadata { cgmIfCond = Just combinedCond } }

mGuardedBy :: TEditLabel -> Maybe Expr -> TEditLabel
mGuardedBy lbl = \case
  Nothing   -> lbl
  Just cond -> lbl `guardedBy` cond

holeWithMCond :: TransId -> Maybe Expr -> EditableSW
holeWithMCond id mCond = SW.Atom $ TEL id (Name $ "_" <> show id) True (CGMetadata Nothing mCond)

-- NOTE: if the label to replaced already has a condition, preserve it
-- TODO: if we ever allow reediting, we will need a smarter way to index holes
fromContinuation
  :: TEditLabel                       -- ^ Label for the transition being replaced
  -> Continuation                 -- ^ Construct to replace the given transition with
  -> Gen TransId EditableSW
fromContinuation (cgmIfCond.trCGMetadata -> mCond) = \case
  Atom{..}   -> pure $ SW.Atom (atomLabel `mGuardedBy` mCond)
  AND{..}    -> pure $ SW.AND (andSplitLabel `mGuardedBy` mCond) andJoinLabel $ mkBranches andBranchLabels
  SimpleLoop{..} -> pure $ SW.SimpleLoop
    (SW.Atom $ sLoopJumpBackLabel    `mGuardedBy` mCond)
    (SW.Atom $ sLoopFallThroughLabel `mGuardedBy` mCond)
  -- TODO: check whether the labelling of the fall-through and jump-back branches are correct
  Loop{..} -> pure $ SW.Loop
    exitLabel
    (SW.Atom $ loopBeforeLabel `mGuardedBy` mCond)
    (SW.Atom $ loopAfterLabel)
    (SW.Atom $ loopJumpBackLabel)
  -- TODO: finish this
  IfXOR{..} -> do
    let thenLabel = ifXorThenLabel `mGuardedBy` mCond
        elseLabel = ifXorElseLabel `mGuardedBy` mCond
    pure $ SW.XOR (SW.Atom thenLabel) (SW.Atom elseLabel)
  UndetXOR  -> do
    lhsId <- gen
    rhsId <- gen
    pure $ SW.XOR (holeWithMCond lhsId mCond) (holeWithMCond rhsId mCond)
  Seq{..} -> do
    lhsId <- gen
    rhsId <- gen
    pure $ SW.Seq
      inbetweenLabel
      (holeWithMCond lhsId mCond)
      (Hole rhsId)
  {- TODO: ACF Continuation

     Handling this will probably require place annotations.
     The user could select a place and we would display which other places
     it can be conencted to.

     Implementation plan:
      1. [DONE] Add place annotations to SafeWorkflows
      2. Implement a function that given a place inside an ACF,
         finds all the other places it can be connected to (inside said ACF).
      3. Implement a function that connects two places in an ACF.
      4. Probably will need a function that "merges" ACFs.
         This will be needed to simplify the structure and coalesce
         nested ACFs into a single one.
         Example use case: See the n-ary XOR above. With the current
         implementation we cannot connect places in different branches,
         because they are inside different ACFs.
  -}
  ACF -> panic "not implemented"

  where
    mkBranch :: TransId -> ANDBranchLabels -> ANDBranch PEditLabel TEditLabel
    mkBranch holeId ANDBranchLabels{..} = ANDBranch inLabel outLabel (Hole holeId)

    mkBranches :: List2 ANDBranchLabels -> List2 (ANDBranch PEditLabel TEditLabel)
    mkBranches = GHC.fromList . zipWith mkBranch [1..] . GHC.toList

-- countHoles :: EditableSW -> Int
-- countHoles = \case
--   sw@(Hole _)               -> 1
--   sw@(SW.Atom _)            -> 0
--   sw@SW.AND{..}             -> sum $ fmap (countHoles . branchWorkflow) andBranches
--   (SW.Loop _ into exit out) -> sum $ map countHoles [into, exit, out]
--   (SW.SimpleLoop exit body) -> sum $ map countHoles [exit, body]
--   sw@(SW.ACF _ acfMap)      -> sum $ M.map (sum . fmap countHoles) acfMap

replaceHole
  :: TransId
  -> Continuation
  -> Builder ()
replaceHole holeId cont = do
  esw  <- gets editableWorkflow
  esw' <- lift $ replaceHoleGenId holeId cont esw
  putESW esw'

replaceHoleGenId
  :: TransId                      -- ^ Look for a hole with this identifier
  -> Continuation                 -- ^ Construct to fill the hole with
  -> EditableSW
  -> Gen TransId EditableSW
replaceHoleGenId holeId cont = \case
  sw@(SW.Atom lbl@TEL{..})
    | trId == holeId && trIsEditable -> fromContinuation lbl cont
  -- TODO: | trId == holeId && not trIsEditable -> panic "..."
    | otherwise -> pure sw
  sw@SW.AND{..} -> do
    andBranches' <- forM andBranches $ \br@ANDBranch{..} -> do
      bfw <- replaceHoleGenId holeId cont branchWorkflow
      pure $ br { branchWorkflow = bfw }
    pure $ sw { andBranches = andBranches' }
  (SW.GenLoop mExitLabel into exit out) -> do
    into' <- traverse (replaceHoleGenId holeId cont) into
    exit' <- replaceHoleGenId holeId cont exit
    out'  <- replaceHoleGenId holeId cont out
    pure $ SW.GenLoop mExitLabel into' exit' out'
  sw@(SW.ACF annots acfMap) -> do
    acfMap' <- mapM (mapM $ replaceHoleGenId holeId cont) acfMap
    pure $ unsafeMkACF annots acfMap'


-----------------------------
-- Control-flow primitives --
-----------------------------

noCode :: CGMetadata
noCode = CGMetadata Nothing Nothing

withoutCond :: Expr -> CGMetadata
withoutCond code = CGMetadata (Just code) Nothing

-- | Finish a hole, replace it with a simple transition.
finish
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Name         -- ^ Name of the new transition
  -> Expr         -- ^ Code for the transition
  -> Builder ()
finish holeId atomName code = do
  transId <- gen
  let cont = Edit.Atom (TEL transId atomName False $ withoutCond code)
  initMethodAnnots atomName
  logAction (replaceHole holeId cont)

-- | Replace a hole with a parallel subworkflow.
parallel
  :: TransId                 -- ^ Identifier of hole to be replaced
  -> Name                   -- ^ Name of splitting transition
  -> Expr                   -- ^ Code for the splitting transitions
  -> Name                   -- ^ Name of joining transition
  -> Expr                   -- ^ Code for the joining transitions
  -> List2 (Name, Name)     -- ^ Names for the places inside the AND-branches
  -> Builder ()
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
  logAction (replaceHole holeId cont)

-- | Replace a hole with a nondeterministically branching subworkflow.
option
  :: TransId
  -> Builder ()
option holeId = do
  logAction (replaceHole holeId Edit.UndetXOR)

-- | Replace a hole with a deterministically branching subworkflow.
conditional
  :: TransId       -- ^ Identifier of hole to be replaced
  -- -> Name          -- ^ Name of the @then@ transition
  -- -> Name          -- ^ Name of the @else@ transition
  -> Expr          -- ^ The condition for the @then@ branch (this will be negated for the @else@ branch)
  -> Builder ()
conditional holeId {- thenName elseName -} thenCond = do
  thenId <- gen
  elseId <- gen
  -- NOTE: since the transitions are editable, the names don't matter for rendering
  let thenLabel = TEL thenId "" True (CGMetadata Nothing $ Just thenCond)
      elseLabel = TEL elseId "" True (CGMetadata Nothing $ Just (neg thenCond))
  logAction (replaceHole holeId (Edit.IfXOR thenLabel elseLabel))

-- | Replace a hole with a "stay-or-continue" construct.
-- Stay in the current state or progress forward.
stayOrContinue
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Expr          -- ^ Condition to satisfy in order to continue
  -> Builder ()
stayOrContinue holeId cond = do
  fallThroughId <- gen
  jumpBackId    <- gen
  let jumpBackLabel    = TEL jumpBackId    "" True (CGMetadata Nothing $ Just (neg cond))
      fallThroughLabel = TEL fallThroughId "" True (CGMetadata Nothing $ Just cond)
  logAction (replaceHole holeId $ Edit.SimpleLoop jumpBackLabel fallThroughLabel)

-- | Replace a hole with an nondeterministic "stay-or-continue" construct.
-- Stay in the current state or progress forward, but the choice is made
-- nondeterministically.
nondetStayOrContinue
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Builder ()
nondetStayOrContinue holeId = do
  fallThroughId <- gen
  jumpBackId    <- gen
  let jumpBackLabel    = TEL jumpBackId    "" True (CGMetadata Nothing Nothing)
      fallThroughLabel = TEL fallThroughId "" True (CGMetadata Nothing Nothing)
  logAction (replaceHole holeId $ Edit.SimpleLoop jumpBackLabel fallThroughLabel)

-- | Replace a hole with a "loop-or-continue" construct.
-- Loop in the the current state with the option to exit.
loopOrContinue
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Expr         -- ^ Condition to exit from the loop
  -> Name         -- ^ Name for the breakpoint
  -> Builder ()
loopOrContinue holeId cond breakPointName = do
  breakPointId <- gen
  beforeId     <- gen
  afterId      <- gen
  jumpBackId   <- gen
  let breakPointLabel = LPlace breakPointName breakPointId
      beforeLabel     = TEL beforeId   "" True (CGMetadata Nothing Nothing)
      afterLabel      = TEL afterId    "" True (CGMetadata Nothing $ Just cond)
      jumpBackLabel   = TEL jumpBackId "" True (CGMetadata Nothing $ Just (neg cond))
  logAction (replaceHole holeId $ Edit.Loop breakPointLabel beforeLabel afterLabel jumpBackLabel)

-- | Replace a hole with an nondeterministic "loop-or-continue" construct.
-- Loop in the the current state with the option to exit, but the choice
-- is made nondeterministically.
nondetLoopOrContinue
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Name         -- ^ Name for the breakpoint
  -> Builder ()
nondetLoopOrContinue holeId breakPointName = do
  breakPointId <- gen
  beforeId     <- gen
  afterId      <- gen
  jumpBackId   <- gen
  let breakPointLabel = LPlace breakPointName breakPointId
      beforeLabel     = TEL beforeId   "" True (CGMetadata Nothing Nothing)
      afterLabel      = TEL afterId    "" True (CGMetadata Nothing Nothing)
      jumpBackLabel   = TEL jumpBackId "" True (CGMetadata Nothing Nothing)
  logAction (replaceHole holeId $ Edit.Loop breakPointLabel beforeLabel afterLabel jumpBackLabel)


-- | Replace a hole wth a sequence of two subworkflows.
sequence
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Name         -- ^ Name for the place in-between the the two subworklows
  -> Builder ()
sequence holeId inbetweenName = do
  inbetweenId <- gen
  let cont = Edit.Seq (LPlace inbetweenName inbetweenId)
  logAction (replaceHole holeId cont)

-- TODO: See "ACF Continutation" note in Language.FCL.SafeWorkflow.Editable
acf
  :: TransId       -- ^ Identifier of hole to be replaced
  -> Builder ()
acf = panic "not implemented"

----------------------------------
-- Method annotation primitives --
----------------------------------

neg :: Expr -> Expr
neg = EUnOp (noLoc Not) . noLoc

role :: Name -> Expr
role = EVar . noLoc

initMethodAnnots
  :: Name
  -> Builder ()
initMethodAnnots methodName = do
  s@CGInfo{..} <- get
  when (methodName `M.notMember` methodAnnotations) $
    put $ s { methodAnnotations = M.insert methodName mempty methodAnnotations }

-- TODO: add logging
addPrecondition
  :: Name
  -> Precondition
  -> Expr
  -> Builder ()
addPrecondition methodName precondType precondExpr = do
  s@CGInfo{..} <- get
  let methodAnnots' = M.insertWith (<>) methodName
        (fromPreconds [(precondType, noLoc precondExpr)])
        methodAnnotations
  put $ s { methodAnnotations = methodAnnots' }

addRole
  :: Name
  -> Name
  -> Builder ()
addRole methodName roleName =
  addPrecondition methodName PrecRoles (role roleName)

addArgs
  :: Name
  -> [Arg]
  -> Builder ()
addArgs methodName args = do
  s@CGInfo{..} <- get
  let methodAnnots' = M.insertWith (<>) methodName
        (fromArgs args)
        methodAnnotations
  put $ s { methodAnnotations = methodAnnots' }

addGlobal
  :: Type
  -> Name
  -> Preconditions
  -> Maybe Expr
  -> Builder ()
addGlobal ty name preconds mDefaultVal = do
  s@CGInfo{..} <- get
  let newGlobal = case mDefaultVal of
        Nothing     -> GlobalDefNull ty preconds (noLoc name)
        Just defVal -> GlobalDef     ty preconds name (noLoc defVal)
  put $ s { globalVariables = newGlobal : globalVariables }

addGlobalSimple
  :: Type
  -> Name
  -> Builder ()
addGlobalSimple ty name = addGlobal ty name [] Nothing

addGlobalWithDefault
  :: Type
  -> Name
  -> Expr
  -> Builder ()
addGlobalWithDefault ty name def = addGlobal ty name [] (Just def)

