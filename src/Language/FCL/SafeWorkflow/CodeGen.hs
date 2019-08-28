{-# LANGUAGE RecordWildCards #-}
module Language.FCL.SafeWorkflow.CodeGen
  ( codeGenScript
  ) where

import Protolude

-- import Data.List.List2
import Data.Foldable (foldl1)
import Data.List.NonEmpty (NonEmpty)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import Language.FCL.Pretty (prettyPrint)
import Language.FCL.Prim (PrimOp(..))
import Language.FCL.AST
  ( Name
  , Script(..)
  , Method(..)
  , Transition(..)
  , WorkflowState(..)
  , Place(..)
  , Preconditions(..)
  , Arg

  , Expr(..)
  , Lit(..)
  , LExpr
  , Loc(..)
  , Located(..)
  )
import Language.FCL.SafeWorkflow (AnnTransition(..), AnnTransitions(..), constructAnnTransitions)
import Language.FCL.SafeWorkflow.Editable

-- NOTE: the transition names are needed to disambiguate XOR-split (if vs non-deterministic)
-- NOTE: the workflowstate conversion is only for debugging

type EditTransition = AnnTransition TEditLabel
type EditTransitions = AnnTransitions PEditLabel TEditLabel

type GropedTransitions = Map Name [EditTransition]
type PlaceAnnotations = Map Place PEditLabel

type WFStateWithIds = WorkflowState
type WFStateWithNames = WorkflowState

type TrWithIds = Transition
type TrWithNames = Transition

codeGenScript :: EditableSW -> Script
codeGenScript esw = Script [] [] [] (codeGenMethods esw) []

codeGenMethods :: EditableSW -> [Method]
codeGenMethods eswf = map (codeGenMethod placeAnnots groupedTrs) $ M.keys groupedTrs where

  annTrs :: EditTransitions
  annTrs = constructAnnTransitions LInitial LTerminal eswf

  groupedTrs :: GropedTransitions
  groupedTrs = groupTransitions $ getTransitions annTrs

  placeAnnots :: PlaceAnnotations
  placeAnnots = getPlaceAnnotations annTrs

groupTransitions :: [EditTransition] -> GropedTransitions
groupTransitions = foldl insert mempty . map extractName where

  extractName :: EditTransition -> (Name, EditTransition)
  extractName tr@(AnnTransition (LFinished name _) _) = (name, tr)
  extractName tr = panic "groupTransitions: " -- TODO:

  insert :: GropedTransitions -> (Name, EditTransition) -> GropedTransitions
  insert trs (name, tr) = M.insertWith (++) name [tr] trs

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

-- TODO: fancy transitions
genTransitionCall :: TrWithNames -> LExpr
genTransitionCall (Arrow from to) = noLoc $ ECall (Left TransitionTo) [toExprLit to]

noLoc :: a -> Located a
noLoc = Located NoLoc

toExprLit :: WFStateWithNames -> LExpr
toExprLit = noLoc . ELit . noLoc . LState

trivialCondition :: LExpr
trivialCondition = noLoc . ELit . noLoc . LBool $ True

noLocIf :: LExpr -> LExpr -> LExpr -> LExpr
noLocIf cond lhs rhs = noLoc $ EIf cond lhs rhs

genTransitionCalls :: NonEmpty TrWithNames -> LExpr
genTransitionCalls = foldl1 (noLocIf trivialCondition) . map genTransitionCall

codeGenMethod
  :: PlaceAnnotations
  -> GropedTransitions
  -> Name
  -> Method
codeGenMethod placeAnnots groupedTrs methodName = Method inputState preconditions (noLoc methodName) args body where
  trs :: [EditTransition]
  trs = flip fromMaybe (M.lookup methodName groupedTrs) $
    panic $ "codeGenMethod: transition '" <> show methodName <> "' not found amongst grouped transitions"

  convertedTrs :: [TrWithNames]
  convertedTrs = map (convertTransition placeAnnots . getTrans) $ trs

  body :: LExpr
  body = case convertedTrs of
    [] -> panic $ "codeGenMethod: Empty transition list for method name: " <> show methodName
    [tr] -> genTransitionCall tr
    trs -> genTransitionCalls (NE.fromList trs)

  preconditions :: Preconditions
  preconditions = Preconditions []

  args :: [Arg]
  args = []

  inputState :: WorkflowState
  inputState = case convertedTrs of
    (Arrow from _) : _ -> from
    [] -> panic $ "codeGenMethod: Empty transition list for method name: " <> show methodName
