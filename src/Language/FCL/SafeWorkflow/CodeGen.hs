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

-- NOTE: the transition names are needed to disambiguate XOR-split (if vs non-deterministic)
-- NOTE: the workflowstate conversion is only for debugging

type EditTransition = AnnTransition TEditLabel
type EditTransitions = AnnTransitions PEditLabel TEditLabel

type GroupedTransitions = Map Name [EditTransition]
type PlaceAnnotations = Map Place PEditLabel

type WFStateWithIds = WorkflowState
type WFStateWithNames = WorkflowState

type TrWithIds = Transition
type TrWithNames = Transition

type ETrWithNames = EditTransition

data MethodAnnotation = MethodAnnotation
  { mAnnPreconds  :: Preconditions
  , mAnnArgs      :: [Arg]
  } deriving (Eq, Ord, Show)

type MethodAnnotations = Map Name MethodAnnotation

data CGInfo = CGInfo
  { globalVariables   :: [Def]
  , methodAnnotations :: MethodAnnotations
  , editableWorkflow  :: EditableSW
  } deriving (Eq, Ord, Show)

instance Semigroup MethodAnnotation where
  (<>) (MethodAnnotation preconds1 args1) (MethodAnnotation preconds2 args2) =
    MethodAnnotation (preconds1 <> preconds2) (args1 <> args2)

instance Monoid MethodAnnotation where
  mempty = MethodAnnotation mempty mempty

codeGenScript :: CGInfo -> Script
codeGenScript cgInfo = Script [] [] [] (codeGenMethods cgInfo) []

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

data BranchingErrors = BranchingErrors
  { unexpectedNoConds ::  [TransId]
  } deriving (Eq, Ord, Show)

instance Pretty BranchingErrors where
  ppr BranchingErrors{..} = vsep
    [ "unexpectedNoConds" <+> listOf unexpectedNoConds
    ]

hasAnyBranchingErrors :: BranchingErrors -> Bool
hasAnyBranchingErrors BranchingErrors{..}
  =  not (null unexpectedNoConds)

-- NOTE: no branching errors for non-branching control-flow
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

convertTransition :: PlaceAnnotations -> TrWithIds -> TrWithNames
convertTransition annots (Arrow from to) = Arrow
  (convertWorkflowState annots from)
  (convertWorkflowState annots to)

convertAnnTransition :: PlaceAnnotations -> EditTransition -> ETrWithNames
convertAnnTransition annots (AnnTransition ann tr) =
  AnnTransition ann (convertTransition annots tr)

genUnAnnotTransCall :: TrWithNames -> LExpr
genUnAnnotTransCall (Arrow from to) =
  noLoc $ ECall (Left TransitionTo) [toExprLit to]

toExprLit :: WFStateWithNames -> LExpr
toExprLit = noLoc . ELit . noLoc . LState

trivialCondition :: LExpr
trivialCondition = noLoc . ELit . noLoc . LBool $ True

noLocIf :: LExpr -> LExpr -> LExpr -> LExpr
noLocIf cond lhs rhs = noLoc $ EIf cond lhs rhs

genAnnotTransCall :: ETrWithNames -> LExpr
genAnnotTransCall (AnnTransition TEL{..} tr)
  | CGMetadata (Just code) _ <- trCGMetadata = noLoc $ ESeq
    (noLoc code)
    (genUnAnnotTransCall tr)
genAnnotTransCall annTr = panic $ "genAnnotTransCall: Transition '"
  <> show annTr
  <> "' does not contain code to be generated."

-- TODO: add codegen for cgCode !!
-- QUESTION: should it be here, or in a separate function?
genIfCondTransCalls :: List2 ETrWithNames -> LExpr
genIfCondTransCalls (List2 t1 t2 trs) = foldl alg (noLoc ENoOp) (t1:t2:trs) where
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

-- TODO: add verification step for CGMetadata
-- check whether every transition has some proper code to be generated
codeGenMethod
  :: PlaceAnnotations
  -> GroupedTransitions
  -> MethodAnnotations
  -> Name
  -> Method
codeGenMethod placeAnnots groupedTrs methodAnnots methodName = Method inputState preconditions (noLoc methodName) args body where
  trsWithIds :: [EditTransition]
  trsWithIds = flip fromMaybe (M.lookup methodName groupedTrs) $
    panic $ "codeGenMethod: transition '" <> show methodName <> "' not found amongst grouped transitions"

  convertedTrs :: [ETrWithNames]
  convertedTrs = map (convertAnnTransition placeAnnots) trsWithIds

  body :: LExpr
  body = case convertedTrs of
    [] -> panic $ "codeGenMethod: Empty transition list for method name: " <> show methodName
    [tr] -> genAnnotTransCall tr
    trs -> genIfCondTransCalls . L2.fromList $ trs

  methodAnn :: MethodAnnotation
  methodAnn = flip fromMaybe (M.lookup methodName methodAnnots)
    $ panic "codeGenMethod: Method `" (show methodName :: Text) "' was not found in method annotation map."

  preconditions :: Preconditions
  preconditions = mAnnPreconds methodAnn

  args :: [Arg]
  args = mAnnArgs methodAnn

  inputState :: WorkflowState
  inputState = case convertedTrs of
    (AnnTransition _ (Arrow from _)) : _ -> from
    [] -> panic $ "codeGenMethod: Empty transition list for method name: " <> show methodName

fromPreconds :: Preconditions -> MethodAnnotation
fromPreconds = flip MethodAnnotation mempty

fromArgs :: [Arg] -> MethodAnnotation
fromArgs = MethodAnnotation mempty
