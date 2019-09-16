{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Language.FCL.SafeWorkflow.CodeGen
  ( codeGenScript
  , CGInfo(..)
  , MethodAnnotation(..)

  , fromPreconds
  , fromArgs
  ) where

import Protolude

import Data.List.List2 (List2(..))

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.List2 as L2

import Language.FCL.Pretty (Pretty(..), prettyPrint, vsep, listOf, (<+>))
import Language.FCL.Prim (PrimOp(..))
import Language.FCL.AST
import Language.FCL.SafeWorkflow (AnnTransition(..), AnnTransitions(..), constructAnnTransitions)
import Language.FCL.SafeWorkflow.Editable

{- TODO: MethodIndentifiers

  Add method-identifier to `CGMetadata` and group transitions
  by that identifier instead of by their method names. Also, make sure
  to generate those IDs and edit them correctly during the refinement
  of a hole.
-}

-- NOTE: the transition names are needed to disambiguate XOR-split (if vs non-deterministic)
-- NOTE: the workflowstate conversion is only for debugging

-- | Transition annotated by an edit label.
type EditTransition  = AnnTransition  TEditLabel
-- | Transitions annotated by an edit label.
type EditTransitions = AnnTransitions PEditLabel TEditLabel

-- | `EditTransition`s grouped by their names.
type GroupedTransitions = Map Name [EditTransition]
-- | Places annotated by a place label.
type PlaceAnnotations = Map Place PEditLabel

-- | Workflow state with IDs.
type WFStateWithIds = WorkflowState
-- | Workflow state with names.
type WFStateWithNames = WorkflowState

-- | Transition identified by a number.
type TrWithIds = Transition
-- | Transition identified by a name.
type TrWithNames = Transition

-- | `EditTransition` identified by a name.
type ETrWithNames = EditTransition

-- | Global annotation for methods.
data MethodAnnotation = MethodAnnotation
  { mAnnPreconds  :: Preconditions  -- ^ Method preconditions
  , mAnnArgs      :: [Arg]          -- ^ Method arguments
  , mAnnCode      :: Maybe Expr     -- ^ Branching independent code
  } deriving (Eq, Ord, Show)

-- | Mapping from method names to method annotations.
type MethodAnnotations = Map Name MethodAnnotation

-- | Code generation information.
data CGInfo = CGInfo
  { globalVariables   :: [Def]              -- ^ Global variables.
  , methodAnnotations :: MethodAnnotations  -- ^ Method annotations.
  , editableWorkflow  :: EditableSW         -- ^ The workflow being edited.
  } deriving (Eq, Ord, Show)

-- | Sequence two possible FCL expressions.
mSeq :: Maybe Expr -> Maybe Expr -> Maybe Expr
mSeq Nothing    Nothing    = Nothing
mSeq (Just lhs) Nothing    = Just lhs
mSeq Nothing    (Just rhs) = Just rhs
mSeq (Just lhs) (Just rhs) = Just $ ESeq (noLoc lhs) (noLoc rhs)

instance Semigroup MethodAnnotation where
  (<>) (MethodAnnotation ps1 args1 code1)
       (MethodAnnotation ps2 args2 code2) =
    MethodAnnotation (ps1 <> ps2) (args1 <> args2) (mSeq code1 code2)

-- | Generate FCL code from the code generation information.
codeGenScript :: CGInfo -> Script
codeGenScript cgInfo@CGInfo{..} = Script [] globalVariables []
  (codeGenMethods cgInfo) []

-- | Generate the FCL methods from the code generation information.
codeGenMethods :: CGInfo -> [Method]
codeGenMethods CGInfo{..}
  = map (codeGenMethod placeAnnots groupedTrs methodAnnotations)
  $ M.keys groupedTrs where

  annTrs :: EditTransitions
  annTrs = constructAnnTransitions LInitial LTerminal editableWorkflow

  groupedTrs :: GroupedTransitions
  groupedTrs = groupTransitions $ getTransitions annTrs

  placeAnnots :: PlaceAnnotations
  placeAnnots = getPlaceAnnotations annTrs

-- TODO: See MethodIdentifiers.
-- | Group the transitions by their names.
groupTransitions :: [EditTransition] -> GroupedTransitions
groupTransitions trs = if any hasAnyBranchingErrors branchingErrors
  then panic $ prettyPrint branchingErrors
  else groupedTrs

  where

    branchingErrors :: Map Name BranchingErrors
    branchingErrors = M.map collectBranchingErrors groupedTrs

    groupedTrs :: GroupedTransitions
    groupedTrs = foldl insert mempty . map extractName $ trs

    extractName :: EditTransition -> (Name, EditTransition)
    extractName tr@(AnnTransition TEL{..} _) = (trMethodName, tr)

    insert :: GroupedTransitions -> (Name, EditTransition) -> GroupedTransitions
    insert trs (name, tr) = M.insertWith (++) name [tr] trs

-- | Errors for a conditional branching in a method.
data BranchingErrors = BranchingErrors
  { unexpectedNoConds ::  [TransId] -- ^ A transition with the name of a coditionally branching method does not have a condition in its `CGMetadata`.
  } deriving (Eq, Ord, Show)

instance Pretty BranchingErrors where
  ppr BranchingErrors{..} = vsep
    [ "unexpectedNoConds" <+> listOf unexpectedNoConds
    ]

-- | Are there any branching errors present?
hasAnyBranchingErrors :: BranchingErrors -> Bool
hasAnyBranchingErrors BranchingErrors{..}
  =  not (null unexpectedNoConds)

-- NOTE: no branching errors for non-branching control-flow
-- | Collect all the branching errors for a group of transitions.
-- It returns no branching errors for non-branching control-flow.
collectBranchingErrors :: [EditTransition] -> BranchingErrors
collectBranchingErrors [_] = BranchingErrors []
collectBranchingErrors trs = BranchingErrors
  (mapMaybe mGetNoCondId trs)

  where

    mGetNoCondId :: EditTransition -> Maybe TransId
    mGetNoCondId (AnnTransition TEL{..} _)
      | isNothing (cgmIfCond trCGMetadata) = Just trId
    mGetNoCondId _ = Nothing


-- | Looks up the real names of the `Place`s inside a `WorkflowState`
-- from the `Place` annotation map.
convertWorkflowState :: PlaceAnnotations -> WFStateWithIds -> WFStateWithNames
convertWorkflowState placeAnnots WorkflowState{..}
  = WorkflowState
  . S.map convertIdsToName
  $ places

  where

    convertIdsToName :: Place -> Place
    convertIdsToName PlaceStart = PlaceStart
    convertIdsToName PlaceEnd = PlaceEnd
    convertIdsToName p
      = Place
      . lPlaceName
      . flip fromMaybe (M.lookup p placeAnnots)
      . panic
      $ "convertWorkflowState: Place '" <> prettyPrint p <> "' is not present in the place annotation map."

-- | Convert a `Transition` identified by numbers
-- to a `Transition` identified by names.
convertTransition :: PlaceAnnotations -> TrWithIds -> TrWithNames
convertTransition annots (Arrow from to) = Arrow
  (convertWorkflowState annots from)
  (convertWorkflowState annots to)

-- | Convert a `EditTransition` identified by numbers
-- to an `EditTransition` identified by names.
convertAnnTransition :: PlaceAnnotations -> EditTransition -> ETrWithNames
convertAnnTransition annots (AnnTransition ann tr) =
  AnnTransition ann (convertTransition annots tr)

-- | Generate an transition call from a transition annotated by names.
genUnAnnotTransCall :: TrWithNames -> LExpr
genUnAnnotTransCall (Arrow from to) =
  noLoc $ ECall (Left TransitionTo) [toExprLit to]

-- | Convert a workflow state to an FCL expression.
toExprLit :: WFStateWithNames -> LExpr
toExprLit = noLoc . ELit . noLoc . LState

-- | Always true condition.
trivialCondition :: LExpr
trivialCondition = noLoc . ELit . noLoc . LBool $ True

-- | If-statement without any location information.
noLocIf :: LExpr -> LExpr -> LExpr -> LExpr
noLocIf cond lhs rhs = noLoc $ EIf cond lhs rhs

-- | Generate FCL code from a transition.
-- First generate the code from the `CGMetadata` present in the transition
-- annotation then generate the transition call.
genAnnotTransCall :: ETrWithNames -> LExpr
genAnnotTransCall (AnnTransition TEL{..} tr)
  | CGMetadata (Just code) _ <- trCGMetadata = noLoc $ ESeq
    (noLoc code)
    (genUnAnnotTransCall tr)
genAnnotTransCall annTr = panic $ "genAnnotTransCall: Transition '"
  <> show annTr
  <> "' does not contain code to be generated."

-- | Generate the if conditions from a list of grouped transitions.
genIfCondTransCalls :: List2 ETrWithNames -> LExpr
genIfCondTransCalls (List2 t1 t2 trs) = foldl alg defaultBranch (t2:trs) where
  alg :: LExpr -> ETrWithNames -> LExpr
  alg ast annTr@(AnnTransition TEL{..} tr)
    | CGMetadata _ (Just cond) <- trCGMetadata = noLocIf
      (noLoc cond)
      (genAnnotTransCall annTr)
      ast
  -- NOTE: should never come here
  alg _ annTr = panic $ "genIfCondTransCall: Transition '"
    <> show annTr
    <> "' does not contain an If condition to generate code from."

  defaultBranch = genAnnotTransCall t1

-- TODO: add verification step for CGMetadata
-- check whether every transition has some proper code to be generated
-- | Generate code for a given method from grouped transitions.
codeGenMethod
  :: PlaceAnnotations
  -> GroupedTransitions
  -> MethodAnnotations
  -> Name
  -> Method
codeGenMethod placeAnnots groupedTrs methodAnnots methodName = Method inputState preconditions (noLoc methodName) args wholeBody where
  trsWithIds :: [EditTransition]
  trsWithIds = flip fromMaybe (M.lookup methodName groupedTrs) $
    panic $ "codeGenMethod: transition '" <> show methodName <> "' not found amongst grouped transitions"

  convertedTrs :: [ETrWithNames]
  convertedTrs = map (convertAnnTransition placeAnnots) trsWithIds

  wholeBody :: LExpr
  wholeBody = maybe innerBody (\res -> noLoc $ ESeq res innerBody) $ do
    MethodAnnotation{..} <- M.lookup methodName methodAnnots
    branchIndependentCode <- mAnnCode
    pure $ noLoc branchIndependentCode

  innerBody :: LExpr
  innerBody = case convertedTrs of
    [] -> panic $ "codeGenMethod: Empty transition list for method name: " <> show methodName
    [tr] -> genAnnotTransCall tr
    trs -> genIfCondTransCalls . L2.fromList $ trs

  methodAnn :: MethodAnnotation
  methodAnn = flip fromMaybe (M.lookup methodName methodAnnots) $
    panic $ "codeGenMethod: Method `" <> prettyPrint methodName <> "' was not found in method annotation map."

  preconditions :: Preconditions
  preconditions = mAnnPreconds methodAnn

  args :: [Arg]
  args = mAnnArgs methodAnn

  inputState :: WorkflowState
  inputState = case convertedTrs of
    (AnnTransition _ (Arrow from _)) : _ -> from
    [] -> panic $ "codeGenMethod: Empty transition list for method name: " <> show methodName

-- | Construct method annotations from preconditions.
fromPreconds :: Preconditions -> MethodAnnotation
fromPreconds ps = MethodAnnotation ps mempty Nothing

-- | Construct method annotations from method arguments.
fromArgs :: [Arg] -> MethodAnnotation
fromArgs args = MethodAnnotation mempty args Nothing
