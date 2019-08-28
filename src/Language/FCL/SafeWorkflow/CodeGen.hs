{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Language.FCL.SafeWorkflow.CodeGen
  ( codeGenScript
  ) where

import Protolude

import Data.Function (on)
import Data.List ((!!))
import Data.List.List2 (List2(..))

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.List2 as L2

import Language.FCL.Pretty (Pretty(..), prettyPrint, vsep, listOf, tupleOf, (<+>))
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
    extractName tr@(AnnTransition (LSimple name _) _) = (name, tr)
    extractName tr@(AnnTransition (LIfCond name _ _) _) = (name, tr)
    extractName tr = panic "groupTransitions: " -- TODO:

    insert :: GropedTransitions -> (Name, EditTransition) -> GropedTransitions
    insert trs (name, tr) = M.insertWith (++) name [tr] trs

data BranchingErrors = BranchingErrors
  { multipleDefaults  ::  [TransId]
  , unexpectedHoles   ::  [HoleId]
  , unexpectedSimples ::  [TransId]
  , clashingIndices   ::  [(TransId, TransId)]
  } deriving (Eq, Ord, Show)

instance Pretty BranchingErrors where
  ppr BranchingErrors{..} = vsep
    [ "multipleDefaults:" <+> listOf multipleDefaults
    , "unexpectedHoles" <+> listOf unexpectedHoles
    , "unexpectedSimples" <+> listOf unexpectedSimples
    , "clashingIndices" <+> listOf (map (\(x,y) -> tupleOf [x,y]) clashingIndices)
    ]

hasAnyBranchingErrors :: BranchingErrors -> Bool
hasAnyBranchingErrors BranchingErrors{..}
  =  not (null multipleDefaults)
  || not (null unexpectedHoles)
  || not (null unexpectedSimples)
  || not (null clashingIndices)

collectBranchingErrors :: [EditTransition] -> BranchingErrors
collectBranchingErrors trs = BranchingErrors
  (mapMaybe mGetDefaultCondId trs)
  (mapMaybe mGetHoleId        trs)
  (mapMaybe mGetSimpleId      trs)
  (collectClashingIndices     trs)

  where

    mGetHoleId :: EditTransition -> Maybe HoleId
    mGetHoleId (AnnTransition LHole{..} _) = Just holeId
    mGetHoleId _ = Nothing

    mGetSimpleId :: EditTransition -> Maybe TransId
    mGetSimpleId (AnnTransition LSimple{..} _) = Just simpleId
    mGetSimpleId _ = Nothing

    mGetDefaultCondId :: EditTransition -> Maybe TransId
    mGetDefaultCondId (AnnTransition (LIfCond _ id CDefault) _) = Just id
    mGetDefaultCondId _ = Nothing

    collectClashingIndices :: [EditTransition] -> [(TransId, TransId)]
    collectClashingIndices trs = do
      let idIx = [ (id,ix) | AnnTransition (LIfCond _ id (CExpr _ ix)) _ <- trs ]
      n <- [0     .. length idIx - 1]
      k <- [n + 1 .. length idIx - 1]
      let (id1, ix1) = idIx !! n
          (id2, ix2) = idIx !! k
      if ix1 == ix2 then [(id1,id2)] else []

-- asd :: [EditTransition]
-- asd =
--   [ AnnTransition (LIfCond undefined 0 (CExpr undefined 0)) undefined
--   , AnnTransition (LIfCond undefined 1 (CExpr undefined 1)) undefined
--   , AnnTransition (LIfCond undefined 2 (CExpr undefined 1)) undefined
--   , AnnTransition (LIfCond undefined 3 (CExpr undefined 1)) undefined
--   ]

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

genIfCondTransCalls :: List2 ETrWithNames -> LExpr
genIfCondTransCalls (L2.reverse -> List2 t1 t2 trs) = foldl alg defaultBranch (t2:trs) where
  alg :: LExpr -> ETrWithNames -> LExpr
  alg ast (AnnTransition LIfCond{..} tr)
    | CExpr cond _ <- ifcCond = noLocIf
      (noLoc . condExpr $ ifcCond)
      (genUnAnnotTransCall tr)
      ast
  alg _ annTr = panic $ "genIfCondTransCall: Transition '"
    <> show annTr
    <> "' is not a non-default if-conditioned transition."

  -- NOTE: the deepest branch should always be a "default" branch
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
    trs -> genIfCondTransCalls . L2.fromList . sortIfCondTrs $ trs

  preconditions :: Preconditions
  preconditions = Preconditions []

  args :: [Arg]
  args = []

  inputState :: WorkflowState
  inputState = case convertedTrs of
    (AnnTransition _ (Arrow from _)) : _ -> from
    [] -> panic $ "codeGenMethod: Empty transition list for method name: " <> show methodName

  -- NOTE: partial function, only works if all elements are @IfCond@s
  sortIfCondTrs :: [EditTransition] -> [EditTransition]
  sortIfCondTrs = sortBy (compare `on` ifcCond . getAnnot)
