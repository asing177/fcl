{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- NOTE: Path-insensitive analysis (See checkStatement (... EIf ...))
module Language.FCL.Undefinedness (
  InvalidStackTrace(..),
  IsInitialized(..),
  unusedVars,
  undefinednessAnalysis,
) where

import Protolude

import Algebra.Lattice
   ( Lattice
   , MeetSemiLattice, JoinSemiLattice
   , BoundedMeetSemiLattice(..)
   , (/\), (\/), meets
   )
import Language.FCL.AST hiding (Transition(..), WorkflowState)
import Language.FCL.WorkflowNet
import qualified Language.FCL.Prim as Prim
import Language.FCL.Pretty (Pretty(..), vcat, token, listOf, (<+>), (<$$>), nest, linebreak)
import Language.FCL.Warning (Warning(UnusedVarWarn))

import Data.Aeson as A hiding (Error)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (unlines)


-- Note: 'foldMap' is to be avoided, since the default behavior using the map
-- monoid is to call a right-biased union. However, regarding the
-- 'UndefinednessEnv' values, we want `unionWith (/\)` in every case.

-- | Undefinedness analysis determines whether a variable is always
-- defined during the execution of the workflow. If there is at least one
-- path/trace where it is uninitialized leading up to the use site,
-- then the variable is considered undefined. Furthermore, if we assign
-- an undefined variable to any other variable, the state of that other variable
-- will be erroneous.
--
-- The algorithm also collect all the possible stack traces during the execution
-- of the workflow. This allows for very precise error message generation
-- but results in facotrial (!) time and space complexity.
--
-- The worst case is when we have an AND-split with `n` different branches.
-- This results in `2^n` number of possible states (one for each branch).
-- At the beginning we can choose from `n` different transitions,
-- then after firing that transition, we still have `n-1`
-- left to activate in the next step. Hence the factorial time complexity.
-- Since we also want to store all these traces, the algortihm requires
-- at least `n!` space to run.
undefinednessAnalysis
  :: Script
  -> Either [InvalidStackTrace] [ValidStackTrace]
undefinednessAnalysis script = do
    initialUndefEnv <- initialEnv script
    let initialMarking = mkInitialMarking initialUndefEnv                           :: Map Place UndefinednessEnv
        wfn = createWorkflowNet script initialUndefEnv genMethodUndefEnv            :: WorkflowNet UndefinednessEnv
        allStackTraces = Set.toList (generateStackTraces initialMarking wfn)        :: [StackTrace]
        (allErrs, allSuccesses) = validateStackTraces initialMarking allStackTraces :: ([InvalidStackTrace], [ValidStackTrace])
    case allErrs of
      [] -> Right allSuccesses
      errs@(_:_) -> Left errs
  where
    mkInitialMarking :: a -> Map Place a
    mkInitialMarking = Map.singleton PlaceStart

    genMethodUndefEnv :: ColorTransition UndefinednessEnv
    genMethodUndefEnv = ColorTransition checkMethodUnsafe

    checkMethodUnsafe :: Method ->
                         (UndefinednessEnv -> UndefinednessEnv) ->
                         Map WorkflowState [UndefinednessEnv -> UndefinednessEnv]
    checkMethodUnsafe method = either panic identity . checkMethod method

-------------------------------------------------------------------------------
-- Environment/state used in analysis
-------------------------------------------------------------------------------

-- | The environment we work with maps variable names to the
-- initialized status.
type UndefinednessEnv = Map Name IsInitialized

-- | Add a variable and its undefinedness status to the environment,
-- unless there is already an error there.
--
-- (Motivation: if we have an assignment for variable x whose rhs
-- refers to an uninitialized variable and a later assignment that
-- assigns a literal to the same variable x, the variable x should
-- still be in an error state. Later valid statements should not make
-- us forget earlier invalid statements.)
initializeInEnv
  :: Name
  -> IsInitialized
  -> UndefinednessEnv
  -> UndefinednessEnv
initializeInEnv = Map.insertWith replaceUnlessError
  where
    -- New value should be inserted unless the old value is an
    -- error. (This is not a "meet" as this operation is allowed to
    -- change an uninitialized value into an initialized one.)
    replaceUnlessError new@(Error _) _old = new
    replaceUnlessError _new old@(Error _) = old
    replaceUnlessError new _old = new

-- | Given the initial values of the global variables (or
-- lack thereof), set their undefinedness status in the environment
-- accordingly.
initialEnv :: Script -> Either [InvalidStackTrace] UndefinednessEnv
initialEnv script = handleErrors (fmap ($ mempty) buildEnv)
  where
    buildEnv = foldlM addDef identity (scriptDefs script)

    handleErrors
      :: Either Text UndefinednessEnv
      -> Either [InvalidStackTrace] UndefinednessEnv
    handleErrors (Left err)
      = Left [InvalidStackTrace [] [err]]
    handleErrors (Right env)
      = case collectErrors env of
          [] -> pure env
          errs@(_:_) -> Left [InvalidStackTrace [] errs]

    addDef
      :: (UndefinednessEnv -> UndefinednessEnv)
      -> Def
      -> Either Text (UndefinednessEnv -> UndefinednessEnv)
    addDef mkEnv (GlobalDef _ precs n lexpr) = do
      mkEnv <- checkPreconditions precs mkEnv
      checkAssignment (located lexpr) mkEnv (pure n) lexpr
    addDef mkEnv (GlobalDefNull _ precs ln) = do
      mkEnv <- checkPreconditions precs mkEnv
      pure $ Map.insert (locVal ln) Uninitialized . mkEnv

-------------------------------------------------------------------------------
-- Stack traces
-------------------------------------------------------------------------------

-- | A "stack trace" is essentially a path through the script
-- graph. Any such path is assumed to start from the "initial" state.
type StackTrace = [StackTraceItem]

-- | Since a stack trace is assumed to start from the "initial" state,
-- we only store a list of destinations and method names (edge
-- labels).
data StackTraceItem
  = StackTraceItem
  { method :: Name
  , initialState :: Marking UndefinednessEnv
  , resultState  :: Marking UndefinednessEnv
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON StackTraceItem where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON StackTraceItem where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Valid stack trace
data ValidStackTrace
  = ValidStackTrace
    { validStackTrace :: StackTrace
    -- ^ Path from "initial" state up until here
    , validMarking :: Marking UndefinednessEnv
    -- ^ Set of Markings of places, mapping variable names to initialized status.
    -- We expect every variable to be either initialized or unitialized in all
    -- output markings of each transition: none are in an error state.
    } deriving (Show)

-- | Invalid stack trace
data InvalidStackTrace
  = InvalidStackTrace
    { invalidStackTrace :: StackTrace
    -- ^ Path from "initial" up until we made a reference to an
    -- uninitialized variable
    , invalidErrMsgs :: [Text]
    -- ^ Messages explaining what went wrong
    }
  deriving (Show, Eq, Generic)

instance ToJSON InvalidStackTrace where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON InvalidStackTrace where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Pretty [InvalidStackTrace] where
  ppr []
    = "No errors"
  ppr errs@(_:_)
    = vcat . map (\x -> ppr x <> linebreak) . List.nub $ errs

instance Pretty InvalidStackTrace where
  ppr (InvalidStackTrace [] msgs)
    = vcat
    $ map token msgs
  ppr (InvalidStackTrace strace msgs)
    =    vcat (map token msgs)
    <$$> nest 3 ("Stack trace leading up to error:" <$$> ppr strace)

instance Pretty StackTraceItem where
  ppr (StackTraceItem m src dst)
    = ppr m <+> ":" <+> listOf (Map.keys src) <+> "->" <+> listOf (Map.keys dst)

instance Pretty StackTrace where
  ppr = vcat . map (\x -> ppr x)

-- | Given an initial marking it traverses a given workflow net
-- and collects the possible stack traces. The algorithm will
-- visit every state at most once.
generateStackTraces
  :: Marking UndefinednessEnv
  -> WorkflowNet UndefinednessEnv
  -> Set StackTrace
generateStackTraces initialMarking wfn =
    genStackTraces mempty initialMarking
  where
    -- | Helper function that tracks the visited states.
    genStackTraces :: Set WorkflowState ->          -- ^ Workflow states visited so far
                      Marking UndefinednessEnv ->   -- ^ Current marking
                      Set StackTrace                -- ^ Possible stack traces
    genStackTraces visited marking
      | Set.member (Map.keysSet marking) visited = Set.singleton []
      | null fireableTransitions = Set.singleton []
      -- depth-first firing
      | otherwise = foldMap recursivelyFireTransition fireableTransitions
      where
        fireableTransitions :: [Transition UndefinednessEnv]
        fireableTransitions = enabledTransitions marking wfn

        recursivelyFireTransition :: Transition UndefinednessEnv -> Set StackTrace
        recursivelyFireTransition t@(Transition _ nm _ _) =
          let resultState = fireUnsafe marking t
              newVisited = Set.insert (Map.keysSet marking) visited
           in Set.map (StackTraceItem nm marking resultState :)
                      (genStackTraces newVisited resultState)

validateStackTraces
  :: Marking UndefinednessEnv
  -> [StackTrace]
  -> ([InvalidStackTrace], [ValidStackTrace])
validateStackTraces initMarking straces =
  partitionEithers (map (validateStackTrace initMarking) straces)

-- | The result of the analysis is either a valid stack trace in which
-- there are no references to uninitialized variables, or an invalid
-- stack trace that tells us what went wrong at which point in the
-- contract.
validateStackTrace
  :: Marking UndefinednessEnv
  -> StackTrace
  -> Either InvalidStackTrace ValidStackTrace
validateStackTrace initMarking strace =
    foldl validateStackItem (Right initValidStackTrace) strace
  where
    initValidStackTrace = ValidStackTrace [] initMarking

    -- NOTE: basically just Either monad stuff
    -- | If the trace is invalid, just return it.
    -- if it is valid, but the current item contains errors,
    -- then invalidate it, else continue exploring the valid trace.
    validateStackItem
      :: Either InvalidStackTrace ValidStackTrace
      -> StackTraceItem
      -> Either InvalidStackTrace ValidStackTrace
    validateStackItem eRes sitem@(StackTraceItem nm _ outs) =
        case eRes of
          Left ivst -> Left ivst
          Right vst
            | null stackItemErrs -> Right (extendedValidStackTrace vst)
            | otherwise -> Left (maybeInvalidStackTrace vst stackItemErrs)
      where
        stackItemErrs = concatMap collectErrors $ Map.elems outs

        extendedValidStackTrace vst =
          vst { validMarking = outs
              , validStackTrace = validStackTrace vst ++ [sitem]
              }

        maybeInvalidStackTrace vst errMsgs =
          InvalidStackTrace { invalidStackTrace = validStackTrace vst ++ [sitem]
                            , invalidErrMsgs = errMsgs
                            }

-- Collect erroneous variables from an environment
collectErrors :: UndefinednessEnv -> [Text]
collectErrors env'
  = concat [ displayError v e | (v, Error e) <- Map.toList env' ]
    where
      displayError v e
        = "Variable " <> show (unName v) <> " undefined at:"
          : map (("  - " <>) . showLoc) (Set.toList e)

--------------------------------------------------------------------------------
-- Undefinedness Check
--
--   For each method, the undefinedness check needs to build a set of
--   transition output places and undefinedness envs for each branch in body of
--   the method. Branches potentially happen in `EIf` expressions, where the
--   different branches transition to different output places; Thus, each method
--   can potentially represent a set of transitions that all share the same
--   input places and initial undefinedness environment (built from the variable
--   definitions at the top of an FCL file), but differ in the output places.
--   Not only might they differ in the output places, but also in the
--   undefinedness environment leading up to the 'transitionTo` prim op call
--   that decides the output places for each branch. The entry point to
--   constructing this "Set of sets of pairs of undefinedness environments and
--   output places" is `checkStatement`.
--
--------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- The "domain" of the abstract interpretation
-------------------------------------------------------------------------------

-- | A variable may either be initialized, uninitialized or in an
-- error state (e.g. it has been assigned the value of an
-- uninitialized variable).
data IsInitialized
  = Initialized
  | Uninitialized
  | Error (Set Loc)
    deriving (Show, Eq, Ord, Generic)

instance ToJSON IsInitialized where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON IsInitialized where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance MeetSemiLattice IsInitialized where
  Initialized   /\ Initialized   = Initialized
  Uninitialized /\ Uninitialized = Uninitialized
  Initialized   /\ Uninitialized = Uninitialized
  Uninitialized /\ Initialized   = Uninitialized
  (Error sloc)  /\ (Error sloc') = Error (sloc `Set.union` sloc')
  e@(Error _)   /\ _             = e
  _             /\ e@(Error _)   = e

instance JoinSemiLattice IsInitialized where
  Initialized   \/ Initialized   = Initialized
  Uninitialized \/ Uninitialized = Uninitialized
  Initialized   \/ Uninitialized = Initialized
  Uninitialized \/ Initialized   = Initialized
  (Error sloc)  \/ (Error sloc') = Error (sloc `Set.intersection` sloc')
  (Error _)     \/ isInit        = isInit
  isInit        \/ (Error _)     = isInit

instance BoundedMeetSemiLattice IsInitialized where
  top = Initialized

instance Lattice IsInitialized where

instance Pretty IsInitialized where
  ppr = \case
    Initialized   -> "Initialized"
    Uninitialized -> "Uninitialized"
    Error sloc    -> "Errors:" <+> listOf (map (ppr . showLoc) (Set.toList sloc))

--------------------------------------------------------------------------------
-- Analyis of FCL AST
--------------------------------------------------------------------------------

checkMethod
  :: Method
  -> (UndefinednessEnv -> UndefinednessEnv)
  -> Either Text (Map WorkflowState [UndefinednessEnv -> UndefinednessEnv])
checkMethod method mkEnv = do
  mkEnv <- checkPreconditions (methodPreconditions method) mkEnv
  checkStatement (methodBody method) mkEnv

checkPreconditions
  :: Preconditions
  -> (UndefinednessEnv -> UndefinednessEnv)
  -> Either Text (UndefinednessEnv -> UndefinednessEnv)
checkPreconditions (Preconditions ps) mkEnv
  = foldM (flip checkExpression) mkEnv . map snd $ ps

-- NOTE: empty set means ~ current state
-- | Check a statement's Undefinedness environment. Returns an error whenever
-- you feed it a naked expression. We expect the input to be an assignment,
-- primop call or if-*statement* (and variants thereof).
checkStatement
  :: LExpr -- ^ *statement* to check
  -> (UndefinednessEnv -> UndefinednessEnv)
  -- NOTE: the list is needed because we can reach the same workflow state on multiple different paths
  -> Either Text (Map WorkflowState [UndefinednessEnv -> UndefinednessEnv])
checkStatement (Located loc actual@(ELit _)) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located loc actual@(EVar _)) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located loc actual@EBinOp{}) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located loc actual@(EUnOp _ _)) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located loc actual@(EMap _)) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located loc actual@(ESet _)) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located loc actual@EConstr{}) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)

-- QUESTION: Branching as in transitioning into multiple places (AND-split)?
-- ANSWER: No, branching here means ~ ending with a transition
-- QUESTION: Can `s0` ever be "branching" statement?
-- This is the most involved case, as branching in the first statement makes
-- checking the paths in the second statement non-trivial
checkStatement (Located _ (ESeq s0 s1)) mkEnv = do
    (branches0, rest0) <- sepBranches <$> checkStatement s0 mkEnv
    -- NOTE: tests pass even with this --> when (not $ null branches0) (panic "branches0 can be non-empty")
    branches1 <-
      case concat (Map.elems rest0) of
        -- QUESTION: Can this ever happen? Only when the args list is empty
        -- ANSWER: Nullary functions exist. For example just a side effecting procedure.
        []      -> checkStatement s1 mkEnv
        r@(_:_) -> Map.unionsWith (++) <$> mapM (checkStatement s1) r
    pure $ Map.unionWith (++) branches0 branches1
  where
    -- Split the map in the cases where the branch ends with a transition and
    -- the cases where there is no transition performed
    sepBranches = Map.partitionWithKey $ \k _ -> not (Set.null k)

-- NOTE: Path insensitive, because it merges the result of both branches.
checkStatement (Located _ (EIf c s0 s1)) mkEnv
  = do
  mkEnv' <- checkExpression c mkEnv
  trueVals <- checkStatement s0 mkEnv'
  falseVals <- checkStatement s1 mkEnv'
  pure (Map.unionWith (++) trueVals falseVals)

checkStatement (Located _ (ECase s ms)) mkEnv
  = do
  scrutEnv <- checkExpression s mkEnv
  Map.unionsWith (++) <$>
    mapM (flip checkStatement scrutEnv . matchBody) ms
  -- Set.unions <$> mapM (\m -> checkStatement (matchBody m) newEnv) ms
checkStatement (Located _ (EBefore c s)) mkEnv
  = checkStatement s =<< checkExpression c mkEnv
checkStatement (Located _ (EAfter c s)) mkEnv
  = checkStatement s =<< checkExpression c mkEnv
checkStatement (Located _ (EBetween c0 c1 s)) mkEnv
  = checkStatement s =<< checkExpression c1 =<< checkExpression c0 mkEnv
checkStatement (Located loc (EAssign var rhs)) mkEnv
  = do
  mkEnv' <- checkAssignment loc mkEnv var rhs
  pure (Map.singleton mempty [mkEnv'])
checkStatement (Located _ (ECall efunc args)) mkEnv
  = case efunc of
      -- Currently, helper functions are not allowed to have effects
      -- In the future, we will have to lookup the helper function body
      -- and call 'checkStatement' on it. However, we still check all the
      -- argument expressions to the helper function for undefinedness.
      Right nm
        -- NOTE: this is just fmap ... (just puts it into a list then assigns it to the empty set)
        -> second (Map.singleton mempty . pure) $
             foldM (flip checkExpression) mkEnv args
      Left Prim.Terminate
        -> pure $ Map.singleton (places endState) [mkEnv]
      Left Prim.TransitionTo
        -> case args of
             [Located _ (ELit (Located _ (LState newState)))]
               -> pure $ Map.singleton (places newState) [mkEnv]
             _ -> Left "malformed primop args"
      Left _
        -> second (Map.singleton mempty . pure) $
             foldM (flip checkExpression) mkEnv args
      -- For any other prim op, there is no transition

checkStatement (Located _ ENoOp) mkEnv
  = pure mempty

checkStatement (Located loc EHole) _
  = panic $ "Hole expression at " <> show loc <> " in `checkStatement`"

-- NOTE: propagates information from the rhs to the lhs
-- | Check an assignment
checkAssignment
  :: Loc
  -> (UndefinednessEnv -> UndefinednessEnv)
  -> NonEmpty Name  -- ^ Left-hand side names
  -> LExpr          -- ^ Right-hand side expression
  -> Either Text (UndefinednessEnv -> UndefinednessEnv)
checkAssignment loc g vars rhs = do
    f1 <- checkExpression rhs g
    varsRhs <- expressionVars rhs
    -- traceM (show $ minsertVar varsRhs (f mempty))
    -- NOTE: just collects the info from the rhs and puts it into the lhs
    let f2 = foldr (\name -> ((minsertVar varsRhs name) .)) identity vars
    pure (f2 . f1)
  where
    replaceError (Error _) = Error $ Set.singleton loc
    replaceError x = x

    -- QUESTION: doesn't the empty map stand for a rhs expression without variables?
    -- ANSWER: if it is not in the map _initially_ it means it is either a local variable or a method argument (see: checkVariable)
    -- if the rhs env is empty, assume all are initialized (args & tmp vars)
    minsertVar :: (Set Name) -> Name -> UndefinednessEnv -> UndefinednessEnv
    minsertVar varNms v env = initializeInEnv v varVal env
      where
               -- NOTE: if there are no variabkes on the rhs, we are good
        varVal | Map.null rhsVarsEnv = Initialized
               -- NOTE: we only care about the variables on the rhs
               -- NOTE: if _any_ variable is undefined/erroneous on the rhs, the lhs will be uninitialized/erroneous too
               | otherwise = replaceError (meets (Map.elems rhsVarsEnv))

        rhsVarsEnv = Map.restrictKeys env varNms

    -- Collect all variables in an *expression*. Returns an error
    -- whenever you feed it sequences or assignments.
    expressionVars
      :: LExpr  -> Either Text (Set Name)
    expressionVars (Located loc (ESeq _ _))
      = Left (showAtLoc loc "expected expression")
    expressionVars (Located _ (ELit _))
      = pure Set.empty
    expressionVars (Located _ (EVar v))
      = pure $ Set.singleton (locVal v)
    expressionVars (Located _ (EBinOp _ s0 s1))
      = Set.union <$> expressionVars s0 <*> expressionVars s1
    expressionVars (Located _ (EUnOp _ s))
      = expressionVars s
    expressionVars (Located _ (EIf c s0 s1))
      = Set.unions <$> mapM expressionVars [c, s0, s1]
    expressionVars (Located _ (ECase s ms))
      = Set.unions <$> mapM expressionVars (s : map matchBody ms)
    expressionVars (Located _ (EMap m))
      = collectExprVars $ Map.keys m <> Map.elems m
    expressionVars (Located _ (ESet s))
      = collectExprVars (toList s)
    expressionVars (Located loc EBefore{})
      = Left (showAtLoc loc "expected expression")
    expressionVars (Located loc EAfter{})
      = Left (showAtLoc loc "expected expression")
    expressionVars (Located loc EBetween{})
      = Left (showAtLoc loc "expected expression")
    expressionVars (Located loc EAssign{})
      = Left (showAtLoc loc "expected expression")
    expressionVars (Located _ (ECall _ ss))
      = Set.unions <$> mapM expressionVars ss
    expressionVars (Located _ ENoOp)
      = pure Set.empty
    expressionVars (Located _ (EConstr _ es))
      = Set.unions <$> mapM expressionVars es
    expressionVars (Located loc EHole)
      = panic $ "Hole expression at " <> show loc <> " in `expressionVars`"

    collectExprVars :: [LExpr] -> Either Text (Set Name)
    collectExprVars xs = case partitionEithers $ map expressionVars xs of
        ([],ss) -> pure (Set.unions ss)
        (es@(_:_),_) -> Left $ unlines es

-- | Check whether an "expression" refers to an uninitialized
-- variable. Returns an error whenever you feed it sequences or
-- assignments.
checkExpression
  :: LExpr -- ^ *expression* to check
  -> (UndefinednessEnv -> UndefinednessEnv)
  -> Either Text (UndefinednessEnv -> UndefinednessEnv)
checkExpression (Located loc (ESeq _ _)) _
  = Left (showAtLoc loc (showAtLoc loc "expected expression"))
checkExpression (Located _ (ELit _)) mkEnv
  = pure mkEnv
checkExpression (Located _ (EVar v)) mkEnv
  = pure (checkVariable v . mkEnv)
checkExpression (Located _ (EBinOp _ s0 s1)) mkEnv
  = checkExpression s1 =<< checkExpression s0 mkEnv
checkExpression (Located _ (EUnOp _ s)) mkEnv
  = checkExpression s mkEnv
checkExpression (Located _ (EIf c s0 s1)) mkEnv
  = foldM (flip checkExpression) mkEnv [c, s0, s1]
checkExpression (Located _ (ECase s ms)) mkEnv
  = foldM (flip checkExpression) mkEnv (s : map matchBody ms)
checkExpression (Located loc EBefore{}) _
  = Left (showAtLoc loc "expected t expression")
checkExpression (Located loc EAfter{}) _
  = Left (showAtLoc loc "expected expression")
checkExpression (Located loc EBetween{}) _
  = Left (showAtLoc loc "expected expression")
checkExpression (Located loc EAssign{}) _
  = Left (showAtLoc loc "expected expression")
checkExpression (Located _ (ECall _ ss)) mkEnv
  = foldM (flip checkExpression) mkEnv ss
checkExpression (Located _ ENoOp) mkEnv
  = pure mkEnv
checkExpression (Located _ (EMap m)) mkEnv
  = collectCheckExprs mkEnv $ Map.keys m <> Map.elems m
checkExpression (Located _ (ESet s)) mkEnv
  = collectCheckExprs mkEnv $ toList s
checkExpression (Located _ (EConstr _ es)) mkEnv
  = foldM (flip checkExpression) mkEnv es
checkExpression (Located loc EHole) _
  = panic $ "Hole expression at " <> show loc <> " in `checkExpression`"

-- | 'checkExpression' for lists of expressions
collectCheckExprs
  :: (UndefinednessEnv -> UndefinednessEnv)
  -> [LExpr]
  -> Either Text (UndefinednessEnv -> UndefinednessEnv)
collectCheckExprs mkEnv = foldM (flip checkExpression) mkEnv

-- | If we reference a variable that is not in the map, it is a
-- temporary variable or a method argument, so we can assume it is
-- initialized (otherwise the expression would not be well-scoped,
-- which is currently caught by the type checker).  Otherwise, if it
-- is in the map and undefined, we "upgrade" the variable's value to
-- the error state.
checkVariable :: Located Name -> UndefinednessEnv -> UndefinednessEnv
checkVariable (Located loc var) env =
  case Map.lookup var env of
    Nothing            -> env
    Just Initialized   -> env
    -- QUESTION: Probably a variable referencing itself?
    -- ANSWER: A variable can be uninitalized but then become initilaized.
    --         If we don't use the variable when it is still uninitialized, then everything is fine.
    --         However, if we use it, it becomes erroneous, and stores the location of the use-site.
    --         There is no going back after becoming erroneous.
    Just Uninitialized -> Map.insert var (Error $ Set.singleton loc) env
    Just (Error err)   -> Map.insert var (Error $ Set.insert loc err) env   -- NOTE: just extending the set of errors



-- | Pretty print a location, given some context.
showAtLoc :: Loc -> Text -> Text
showAtLoc NoLoc x   = x
showAtLoc l@Loc{} x = x <> " at " <> showLoc l

-- | Pretty print a location.
showLoc :: Loc -> Text
showLoc NoLoc = "<<no location info available>>"
showLoc (Loc line col) = "line " <> show line <> ":" <> show col

--------------------------------------------------------------------------------
-- Uninitialized variable warnings
--------------------------------------------------------------------------------

-- | Given the stack traces for a script, output warnings for any variables that
-- aren't ever initialised in any execution trace.
unusedVars :: [ValidStackTrace] -> [Warning]
unusedVars
    = map UnusedVarWarn
    . Map.keys
    . Map.filter (== Uninitialized)
    . Map.unionsWith (\/)
    . concatMap (Map.elems . validMarking)
