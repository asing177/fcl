{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Language.FCL.SafeWorkflow.CodeGen
  ( codeGenScript
  ) where

import Protolude

import Data.List.List2 (List2(..))

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.List2 as L2

import Language.FCL.Pretty (Pretty(..), prettyPrint, vsep, listOf, (<+>))
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

type ETrWithNames = EditTransition

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
groupTransitions trs = if any hasAnyBranchingErrors branchingErrors
  then panic $ prettyPrint branchingErrors
  else groupedTrs

  where

    branchingErrors :: Map Name BranchingErrors
    branchingErrors = M.map collectBranchingErrors groupedTrs

    groupedTrs :: GropedTransitions
    groupedTrs = foldl insert mempty . map extractName $ trs

    extractName :: EditTransition -> (Name, EditTransition)
    extractName tr@(AnnTransition TEL{..} _) = (trMethodName, tr)

    insert :: GropedTransitions -> (Name, EditTransition) -> GropedTransitions
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

collectBranchingErrors :: [EditTransition] -> BranchingErrors
collectBranchingErrors trs = BranchingErrors
  (mapMaybe mGetNoCondId trs)

  where

    mGetNoCondId :: EditTransition -> Maybe TransId
    mGetNoCondId (AnnTransition TEL{..} _)
      | isJust (cgmIfCond trCGMetadata) = Just trId
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

noLoc :: a -> Located a
noLoc = Located NoLoc

toExprLit :: WFStateWithNames -> LExpr
toExprLit = noLoc . ELit . noLoc . LState

trivialCondition :: LExpr
trivialCondition = noLoc . ELit . noLoc . LBool $ True

noLocIf :: LExpr -> LExpr -> LExpr -> LExpr
noLocIf cond lhs rhs = noLoc $ EIf cond lhs rhs

-- TODO: add codegen for cgCode !!
-- QUESTION: should it be here, or in a separate function?
genIfCondTransCalls :: List2 ETrWithNames -> LExpr
genIfCondTransCalls (List2 t1 t2 trs) = foldl alg defaultBranch (t2:trs) where
  alg :: LExpr -> ETrWithNames -> LExpr
  alg ast (AnnTransition TEL{..} tr)
    | CGMetadata code (Just cond) <- trCGMetadata = noLocIf
      (noLoc cond)
      (noLoc $ ESeq (noLoc code) $ genUnAnnotTransCall tr)
      ast
  -- NOTE: should never come here
  alg _ annTr = panic $ "genIfCondTransCall: Transition '"
    <> show annTr
    <> "' does not contain an If condition."

  -- NOTE: Since the conditions are assumed to be mutually exclusive
  -- and should cover the entire event space, we can pick any branch
  -- to be the default one.
  defaultBranch :: LExpr
  defaultBranch = genUnAnnotTransCall . getTrans $ t1

codeGenMethod
  :: PlaceAnnotations
  -> GropedTransitions
  -> Name
  -> Method
codeGenMethod placeAnnots groupedTrs methodName = Method inputState preconditions (noLoc methodName) args body where
  trsWithIds :: [EditTransition]
  trsWithIds = flip fromMaybe (M.lookup methodName groupedTrs) $
    panic $ "codeGenMethod: transition '" <> show methodName <> "' not found amongst grouped transitions"

  convertedTrs :: [ETrWithNames]
  convertedTrs = map (convertAnnTransition placeAnnots) trsWithIds

  body :: LExpr
  body = case convertedTrs of
    [] -> panic $ "codeGenMethod: Empty transition list for method name: " <> show methodName
    [tr] -> genUnAnnotTransCall . getTrans $ tr
    trs -> genIfCondTransCalls . L2.fromList $ trs

  preconditions :: Preconditions
  preconditions = Preconditions []

  args :: [Arg]
  args = []

  inputState :: WorkflowState
  inputState = case convertedTrs of
    (AnnTransition _ (Arrow from _)) : _ -> from
    [] -> panic $ "codeGenMethod: Empty transition list for method name: " <> show methodName

