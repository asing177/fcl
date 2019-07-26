{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- TODO: investigate one-step AND global loops

module Language.FCL.Reachability.SplitAndMerge
  ( module Language.FCL.Reachability.Definitions
  , checkTransitions
  , reachabilityGraph
  , coreachabilityGraph
  , freeChoicePropertyViolations
  ) where

import Protolude

import Control.Monad.RWS.Strict
import qualified Control.Monad.Trans.Reader

import Data.Set (Set)
import Data.Sequence (Seq(..))
import Data.List.NonEmpty (NonEmpty(..), (<|))

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq
import qualified Data.List.NonEmpty as NE

import Language.FCL.AST (Name(..), Place(..), Transition(..), WorkflowState, endState, places, startState, unsafeWorkflowState, makeWorkflowState, isSubWorkflow, wfUnion, (\\))
import Language.FCL.Reachability.Definitions
import Language.FCL.Reachability.Utils
import Language.FCL.Reachability.StructuredTransition

-- TODO: remove these
import Language.FCL.Debug
import Language.FCL.Pretty

-- | Tags for detecting loops.
data Status = BeingExplored
            | Looping
            | CanProgress
  deriving (Eq, Ord, Show)

instance Semigroup Status where
  (<>) CanProgress _ = CanProgress
  (<>) _ CanProgress = CanProgress
  (<>) BeingExplored x = x
  (<>) x BeingExplored = x
  (<>) Looping Looping = Looping

instance Monoid Status where
  mempty = BeingExplored

instance Pretty Status where
  ppr BeingExplored = "BeingExplored"
  ppr Looping = "Looping"
  ppr CanProgress = "CanProgress"

-- | Assigns a status tag for each visited workflow state.
type StatusMap = Map WorkflowState Status

-- | Locally visited nodes inside an AND branch.
type ANDSplitContext = Set WorkflowState

-- | State for constructing a reachability graph for a free choice net.
data BuilderState
  = BuilderState
  { bsReachabilityGraph :: ReachabilityGraph        -- ^ The reachability graph itself
  , bsStatusMap         :: StatusMap                -- ^ Marking each state with its current exploration status
  , bsLocalContexts     :: NonEmpty ANDSplitContext -- ^ Non-empty stack of local contexts for AND-split branches
  } deriving (Eq, Ord, Show)

-- | Reachability graph builder monad for free choice nets
type FreeChoiceGBM = RWS
  [SimpleTransition]            -- R: the structured transitions of the workflow
  (Set WFError, Set Transition) -- W: errors and used transitions
  BuilderState                  -- S: the state for building the reachability graph

-- | Given a set of transitions, check whether they describe a sound free choice workflow.
checkTransitions :: Set Transition -> Either [WFError] ReachabilityGraph
checkTransitions ts = case first S.toList $ reachabilityGraph ts of
    ([], graph) -> Right graph
    (errs@(_:_), _) -> Left errs


-- | Build a reachability graph. When the set of errors is empty,
-- then the workflow is sound. This graph does not
-- contain any intermediate states across AND branches.
reachabilityGraph :: Set Transition -> (Set WFError, ReachabilityGraph)
reachabilityGraph declaredTransitions =
  case freeChoicePropertyViolations declaredTransitions of
    []   -> (allErrs, graph)
    errs -> (S.fromList errs, mempty)

  where
    graph :: ReachabilityGraph
    stateErrs :: Set WFError
    usedTransitions :: Set Transition
    (_, BuilderState graph statuses _, (stateErrs, usedTransitions))
      = runRWS
          (buildGraph startState)                     -- Function to run in RWS monad
          (structureSimply $ S.toList declaredTransitions)  -- The static context
          (BuilderState mempty mempty (mempty :| []))                    -- The dynamic context

    allErrs :: Set WFError
    allErrs = mconcat [stateErrs, loopingStateErrs, prunedCoreachabilityErrs, unreachableTransitionErrs]
      where
        unreachableTransitionErrs
          = S.map UnreachableTransition $ declaredTransitions S.\\ usedTransitions

        loopingStateErrs :: Set WFError
        loopingStateErrs = S.fromList
                         . map (Counreachable . fst)
                         . filter ((==Looping) . snd)
                         . M.toList
                         $ analyzeLoops graph statuses

        erroneousState :: WFError -> Maybe WorkflowState
        erroneousState (NotOneBounded wfSt _ _)      = Just wfSt -- ^ incorrect transitions
        erroneousState (ImproperCompletion wfSt _ _) = Just wfSt -- ^ incorrect transitions
        erroneousState (LoopingANDBranch wfSt _)     = Just wfSt -- ^ erroneous AND branch loops
        erroneousState _ = Nothing

        erroneousStates :: [WorkflowState]
        erroneousStates = mapMaybe erroneousState
                        . S.toList
                        $ stateErrs

        -- Exclude those states from where we possibly couldn't transition into another state
        -- because the transition itself was erroneous
        prunedCoreachabilityErrs :: Set WFError
        prunedCoreachabilityErrs = S.fromList $
          [ e | e@(Counreachable wfSt) <- S.toList coreachabilityErrs
              , wfSt `notElem` erroneousStates
          ]

        coreachabilityErrs :: Set WFError
        coreachabilityErrs = S.map Counreachable counreachable
          where
            -- We have the set 'allCounreachable', but that is too much
            -- information for the user. We only want to report the "outermost"
            -- offending nodes, since all their predecessors are counreachable
            -- by transitivity. We do a breadth-first graph traversal to achieve
            -- this.
            counreachable :: Set WorkflowState
            counreachable
              | null allCounreachable = mempty
              | otherwise = case go (mempty,mempty) (Sq.singleton startState) of
                  xs
                    | isNonEmpty xs && all (`elem` allCounreachable) xs -> -- do a sanity check
                        xs
                    | otherwise ->
                        panic $ "The workflow is unsound, but so is the soundness check."
                              <> show xs <> "\n" <> show allCounreachable
              where
                -- Construct the breadth-first spanning tree of the reachability
                -- graph in order to only report relevant coreachability errors,
                -- i.e. the leaves of the tree. NB: @seen@ refers to the seen
                -- counreachable states, not all seen states.
                go
                  :: (Set WorkflowState, Set WorkflowState)
                  -> Seq WorkflowState
                  -> Set WorkflowState
                go (_,leaves) Empty = leaves -- the counreachable nodes to report
                go (oldSeen, leaves) (nextOnes :|> curr)
                  = case unseenNeighbours of
                      ns@(_:_) ->
                        go (seen, leaves) (Sq.fromList ns <> nextOnes)
                      []
                        | curr `elem` allCounreachable ->
                            go (seen, S.insert curr leaves) nextOnes
                        | otherwise -> -- all neighbours already seen or end state
                            go (seen, leaves) nextOnes
                  where
                    seen :: Set WorkflowState
                    seen = S.insert curr oldSeen

                    unseenNeighbours :: [WorkflowState]
                    unseenNeighbours
                      = filter (not . (`elem` seen))
                      . S.toList
                      . fromMaybe mempty
                      $ M.lookup curr graph
                                       -- NOTE: may not be safe anymore
                                       -- safe because we only ever go to
                                       -- successor nodes and these have to be
                                       -- in the reachability graph

                allCounreachable, coreachable, reachable :: Set WorkflowState
                allCounreachable = reachable S.\\ coreachable
                -- coreachable = gatherReachableStatesFrom endState (coreachabilityGraph graph)
                coreachable = gatherReachableStatesFrom endState (coreachabilityGraph graph)
                reachable = gatherReachableStatesFrom startState graph

-- Given a reachability graph, make a coreachability graph:
-- 1. reverse all the arrows in the reachability graph
-- TODO: remove this outdated comment --> 2. traverse the graph from the end state and collect all reachable states
coreachabilityGraph :: ReachabilityGraph -> ReachabilityGraph
coreachabilityGraph rGraph = M.fromListWith (<>)
  [ (dst, S.singleton src)
  | (src, dsts) <- M.toList rGraph
  , dst <- S.toList dsts
  ]


--------------------------------------------------------------------------------
-- Convenience functions, these are not exported.
--------------------------------------------------------------------------------

maybeLog :: Monad m => Text -> m ()
maybeLog msg = pure ()

pprD :: Pretty (Debug a) => a -> Doc
pprD = ppr . Debug

newtype PP a = PP { getPrettiable :: a }

instance Pretty (PP StatusMap) where
  ppr = vcat
      . (text "Status Map:" :)
      . map (\(k, status) -> ppr (Debug k) <+> text "->" <+> ppr status)
      . M.toList
      . getPrettiable

buildGraph :: WorkflowState -> FreeChoiceGBM [WorkflowState]
buildGraph curr = do
  unvisited <- unvisitedM curr
  trs       <- applicableTransitions curr
  locals    <- getLocalContext
  let noApplicableTransitions  = null trs
      hasApplicableTransitions = not (null trs)

  sm <- gets bsStatusMap
  maybeLog $ show $ "Current:" <+> pprD curr
  maybeLog $ show $ indent 2 $ vsep
    [ "visited:" <+> ppr (not unvisited)
    , "no app. trs.:" <+> ppr noApplicableTransitions
    , "locals:" <+> bracketList locals
    , ppr (PP sm)
    ]
  maybeLog ""

  -- new state, can continue
  x <- if unvisited && hasApplicableTransitions then do
    setStatus curr BeingExplored
    addToLocalContext curr
    -- NOTE: possible transitions to apply
    xorResults <- forM trs $ \case
      t@NoSplit{} -> do
        checkSimpleTransitionM curr t
        let next = applySimpleTransition curr t
        modifyGraph $ M.insertWith S.union curr (S.singleton next)
        buildGraph next
      t@(ANDSplit input ps) -> do
        checkSimpleTransitionM curr t
        let next = applySimpleTransition curr t
            leftover = curr \\ input
        modifyGraph $ M.insertWith S.union curr (S.singleton next)
        -- singleton local states of each branch
        let localStates = map (unsafeWorkflowState . S.singleton) ps

        allBranchesAreSingletons <- null <$> concatMapM applicableTransitions localStates
        continuations <- if allBranchesAreSingletons then
          buildGraph next
        else do
          -- NOTE: continue exploring the graph locally
          setStatus next BeingExplored
          (individualResults :: [[WorkflowState]]) <- forM localStates $ \lclSt -> do
            pushNewLocalContext
            addToLocalContext lclSt
            r <- buildGraph lclSt
            when (null r) $
              yell $ LoopingANDBranch input lclSt
            popLocalContext
            pure r
          -- NOTE: during the merge, we need to track errors
          (mergedResult :: [WorkflowState]) <- foldlM (cartesianWithM checkedWfUnionM) [leftover] individualResults

          modifyGraph $ M.insertWith S.union next (S.fromList mergedResult)
          concatMapM buildGraph mergedResult

        sm <- gets bsStatusMap

        let statuses = mapMaybe (`M.lookup` sm) continuations

        maybeLog $ show $ "Merging at:" <+> pprD next
        maybeLog $ show $ indent 2 $ vsep
          [ "curr:" <+> pprD curr
          -- , "merged:" <+> (bracketList $ map pprD mergedResult)
          , ppr (PP sm)
          , "statuses:" <+> bracketList statuses
          ]
        maybeLog ""

        if BeingExplored `elem` statuses then
          panic $ show $ "When exploring state '" <> pprD next <> "' the returned statuses of the XOR branches contained 'BeingExplored'"
        else if CanProgress `elem` statuses then
          setStatus next CanProgress
        else
          setStatus next Looping

        pure continuations

    sm <- gets bsStatusMap

    let xorResults' = concat xorResults
        statuses    = mapMaybe (`M.lookup` sm) xorResults'

    if BeingExplored `elem` statuses then
      panic $ show $ "When exploring state '" <> pprD curr <> "' the returned statuses of the XOR branches contained 'BeingExplored'"
    else if CanProgress `elem` statuses then
      setStatus curr CanProgress
    else
      setStatus curr Looping

    pure xorResults'

  -- new state, can't continue
  else if unvisited && noApplicableTransitions then do
    setStatus curr CanProgress
    modifyGraph $ M.insert curr mempty
    addToLocalContext curr
    pure [curr]
  else if (not unvisited) && noApplicableTransitions && (curr `notElem` locals) then do
    maybeLog $ show $ (ppr curr) <+> "is global loop"
    yell $ ANDBranchGlobalExit curr
    pure [curr]
  else if (not unvisited) && noApplicableTransitions && (curr `elem` locals) then do
    pure [curr]
  -- visited an already visited state
  else do
    sm <- gets bsStatusMap

    maybeLog $ show $ "Current:" <+> pprD curr
    maybeLog $ show $ indent 2 $ vsep
      [ ppr (PP sm)
      ]
    maybeLog ""

    case M.lookup curr sm of
      Nothing -> panic $ show $
        "buildGraph: state '" <> pprD curr <>
        "' has already been visited but does not have an entry in the status map"
      Just CanProgress   ->
        if curr `elem` locals then
          -- TODO: or just pure [] instead of pure [curr]?
          pure []
        else do
          -- TODO: is the error message needed here?
          yell $ ANDBranchGlobalExit curr
          pure [curr]
      Just Looping       ->
        if curr `elem` locals then
          pure []
        else do
          -- TODO: can't we just pure [curr]?
          yell $ ANDBranchGlobalExit curr
          pure []
      Just BeingExplored -> do
        setStatus curr Looping
        if curr `elem` locals then
          pure []
        else do
          -- TODO: can't we just pure [curr]?
          yell $ ANDBranchGlobalExit curr
          pure []
  pure x

-- | Collects the applicable transitions in a given workflow state.
applicableTransitions :: WorkflowState -> FreeChoiceGBM [SimpleTransition]
applicableTransitions curr =
  filter ((`isSubWorkflow` curr) . inputState) <$> ask

-- | Checks whether a workflow state is globally unvisited.
unvisitedM :: WorkflowState -> FreeChoiceGBM Bool
unvisitedM s = do
  graph <- gets bsReachabilityGraph
  pure $ s `M.notMember` graph

-- | Checks whether a transtion can be applied to a given state.
-- If it would result in an error, then it reports the corresponding error.
checkSimpleTransitionM :: WorkflowState -> SimpleTransition -> FreeChoiceGBM (Maybe WFError)
checkSimpleTransitionM curr t = do
  let t'   = unstructureSimpleTransition t
      mErr = checkTransition curr t'
  tell (mempty, S.singleton t')
  case mErr of
    Nothing  -> pure mErr
    Just err -> yell err >> pure mErr

-- | Applies a transition to a given state.
applySimpleTransition :: WorkflowState -> SimpleTransition -> WorkflowState
applySimpleTransition curr t = (curr \\ src) `wfUnion` dst where
  Arrow src dst = unstructureSimpleTransition t
{-# INLINE applySimpleTransition #-}

-- | Unions two workflow states and gathers all errors that may arise.
checkedWfUnion :: WorkflowState -> WorkflowState -> Either WFError WorkflowState
checkedWfUnion lhs rhs
  | not $ null sharedPlaces = Left (NotOneBoundedMerge lhs rhs sharedPlaces)
  | PlaceEnd `S.member` lhsPlaces || PlaceEnd `S.member` rhsPlaces
  , S.size (lhsPlaces `S.union` rhsPlaces) > 1
  = Left (ImproperCompletionMerge lhs rhs)
  | otherwise = Right (lhs <> rhs)
  where sharedPlaces = lhsPlaces `S.intersection` rhsPlaces
        lhsPlaces = places lhs
        rhsPlaces = places rhs

-- | Unions two workflow states and reports all errors that may arise in a monadic context.
checkedWfUnionM :: WorkflowState -> WorkflowState -> FreeChoiceGBM WorkflowState
checkedWfUnionM lhs rhs = case checkedWfUnion lhs rhs of
  Right wfSt -> pure wfSt
  Left  err  -> yell err >> pure (lhs <> rhs)

-- | Determines whether a set of transitions representing a workflow
-- satisfy the free choice property.
freeChoicePropertyViolations :: Set Transition -> [WFError]
freeChoicePropertyViolations (S.toList -> trs)
  = catMaybes [ sharedInputPlaces lhs rhs
              | (k, lhs) <- zip [0..] trs
              , rhs <- drop k trs
              ]

-- | Determines whether a pair of transitions satisfy the free choice property.
sharedInputPlaces :: Transition -> Transition -> Maybe WFError
sharedInputPlaces t1@(Arrow lhs _) t2@(Arrow rhs _)
  | lhsPreset <- places lhs
  , rhsPreset <- places rhs
  = if S.disjoint lhsPreset rhsPreset || lhsPreset == rhsPreset then
      Nothing
    else
      Just (FreeChoiceViolation t1 t2 (S.intersection lhsPreset lhsPreset))

-- | A function combining every element from the first list
-- with every element in the second list.
cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianWith f xs ys = [f x y | x <- xs, y <- ys]

-- | Monadic version of `cartesianWith`.
cartesianWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
cartesianWithM f xs = sequence . cartesianWith f xs

-- | General modifying function for the reachability graph.
modifyGraph :: (ReachabilityGraph -> ReachabilityGraph) -> FreeChoiceGBM ()
modifyGraph f = do
  bs@BuilderState{..} <- get
  put $ bs { bsReachabilityGraph = f bsReachabilityGraph }

-- | General modifying function for the reachability graph.
modifyStatus :: (StatusMap -> StatusMap) -> FreeChoiceGBM ()
modifyStatus f = do
  bs@BuilderState{..} <- get
  put $ bs { bsStatusMap = f bsStatusMap }

setStatus :: WorkflowState -> Status -> FreeChoiceGBM ()
setStatus wf st = modifyStatus $ M.insert wf st

-- | General modifying function for the current local context.
modifyLocalContext :: (ANDSplitContext -> ANDSplitContext) -> FreeChoiceGBM ()
modifyLocalContext f = do
  bs@BuilderState{..} <- get
  case bsLocalContexts of
    x :| xs -> put $ bs { bsLocalContexts = f x :| xs}

-- | Creates a new empty local context (for an AND branch).
pushNewLocalContext :: FreeChoiceGBM ()
pushNewLocalContext = do
  bs@BuilderState{..} <- get
  put $ bs { bsLocalContexts = mempty <| bsLocalContexts }

-- | Adds a given state to the current local context.
addToLocalContext :: WorkflowState -> FreeChoiceGBM ()
addToLocalContext wfSt = modifyLocalContext (S.insert wfSt)

-- | Removes the current local context.
popLocalContext :: FreeChoiceGBM ()
popLocalContext = do
  bs@BuilderState{..} <- get
  case bsLocalContexts of
    _:|xs | not (null xs) -> put $ bs { bsLocalContexts = NE.fromList xs }
    otherwise             -> panic $ "Reachability: can't pop last element from context stack"

-- | Acquires the current local context.
getLocalContext :: FreeChoiceGBM ANDSplitContext
getLocalContext = do
  bs@BuilderState{..} <- get
  case bsLocalContexts of
    x :| _ -> pure x

-------------------------
-- Local loop analysis --
-------------------------

-- | Monad for loop analysis. It keeps track of the
-- reachability graph to know where the information can flow,
-- and updates a status map in each iteration.
type LoopAnalysisM = ReaderT ReachabilityGraph (State StatusMap)

-- | Running the loop analysis.
runLoopAnalysis :: ReachabilityGraph -> StatusMap -> LoopAnalysisM a -> (a,StatusMap)
runLoopAnalysis graph statuses =
  flip runState statuses . flip runReaderT graph

-- | Executing the loop analysis.
execLoopAnalysis :: ReachabilityGraph -> StatusMap -> LoopAnalysisM a -> StatusMap
execLoopAnalysis graph statuses = snd . runLoopAnalysis graph statuses

-- | Data-flow analysis for detecting local loops.
-- It propagates information bacward in the reachability graph.
-- If we can progress forward from a given state, we can also
-- progress forward from any predecessor of it.
analyzeLoops :: ReachabilityGraph -> StatusMap -> StatusMap
analyzeLoops graph statuses = execLoopAnalysis (coreachabilityGraph graph) statuses
                            $ untilFixedM propagateInfoM
  where

  -- | Run the analysis until it reaches a fixed point.
  untilFixedM :: LoopAnalysisM StatusMap -> LoopAnalysisM StatusMap
  untilFixedM analyze = do
    old <- get
    new <- analyze
    maybeLog $ show $ ppr $ old
    if old == new then do
      maybeLog $ show $ ppr old
      pure old
    else
      untilFixedM analyze

  -- | One iteration of the analysis. Propagates information.
  propagateInfoM :: LoopAnalysisM StatusMap
  propagateInfoM = do
    cograph <- ask
    forM_ (M.toList cograph) $ \(curr, tos) -> do
      status <- gets (M.findWithDefault mempty curr)
      forM_ tos $ \to -> do
        modify $ M.adjust (<> status) to
    get
