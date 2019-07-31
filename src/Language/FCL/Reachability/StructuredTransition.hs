{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Language.FCL.Reachability.StructuredTransition
  ( SimpleTransition(NoSplit)
  , StructuredTransition(Single)
  , pattern XORTransitions
  , pattern ANDSplit

  , mkSimpleTransition
  , unstructureSimpleTransition
  , mkStructuredTransition
  , unstructureTransition

  , inputState
  , outputStates

  , TransitionsGroup
  , structureSimply
  , unstructureSimply
  , structureTransitions
  , unstructureTransitions

  ) where

import Protolude hiding (Complex, toList)

import qualified Data.Set as S

import Data.List.List2

import Language.FCL.AST
import Language.FCL.Debug (Debug(..))
import Language.FCL.Pretty ((<+>), sqppr)

data SimpleTransition where
  NoSplit   :: WorkflowState -> Place       -> SimpleTransition
  ANDSplit' :: WorkflowState -> List2 Place -> SimpleTransition
  deriving (Eq, Ord, Show)

-- | Type-safe way of representing transitions tagged with their "sort" (`single`, `AND`, `XOR`).
data StructuredTransition where
  Single   :: SimpleTransition       -> StructuredTransition
  XORSplit :: List2 SimpleTransition -> StructuredTransition
  deriving (Eq, Ord, Show)

-- | Pattern synonym for conveniently getting
-- the possible transitions in a XOR-split.
pattern XORTransitions :: [SimpleTransition] -> StructuredTransition
pattern XORTransitions trs <- XORSplit (L2 trs)
{-# COMPLETE XORTransitions #-}

pattern ANDSplit :: WorkflowState -> [Place] -> SimpleTransition
pattern ANDSplit wf ps <- ANDSplit' wf (L2 ps)
{-# COMPLETE NoSplit, ANDSplit #-}

inputState :: SimpleTransition -> WorkflowState
inputState (NoSplit wfSt _)  = wfSt
inputState (ANDSplit' wfSt _) = wfSt

outputStates :: SimpleTransition -> [WorkflowState]
outputStates (NoSplit _ p)           = [unsafeWorkflowState (S.singleton p)]
outputStates (ANDSplit' _ (L2 ps)) = [unsafeWorkflowState $ S.fromList ps]

possibleTransitions :: StructuredTransition -> [SimpleTransition]
possibleTransitions (Single tr) = [tr]
possibleTransitions (XORSplit (L2 trs)) = trs

groupByInputs :: [Transition] -> [TransitionsGroup]
groupByInputs = groupBy sameInput where
  sameInput :: Transition -> Transition -> Bool
  sameInput (Arrow input1 _) (Arrow input2 _) = input1 == input2

-- Transitions grouped by their input states
type TransitionsGroup = [Transition]

mkSimpleTransition :: Transition -> SimpleTransition
mkSimpleTransition tr@(Arrow lhs rhs)
  | [p]        <- S.toList . places $ rhs = NoSplit  lhs p
  | (p1:p2:ps) <- S.toList . places $ rhs = ANDSplit' lhs (List2 p1 p2 ps)
  | otherwise = panic errMsg where
    errMsg = show
      $ "mkSimpleTransition: Can't construct simple transition from"
      <+> sqppr (Debug tr)

mkStructuredTransition :: TransitionsGroup -> StructuredTransition
mkStructuredTransition [] = panic "mkStructuredTransition: can't construct structured transition from an empty list of transitions"
mkStructuredTransition [tr] = Single $ mkSimpleTransition tr
mkStructuredTransition trs@(Arrow lhs _ : _) = XORSplit $ fromList $ map mkSimpleTransition trs

structureTransitions :: [Transition] -> [StructuredTransition]
structureTransitions = map mkStructuredTransition . groupByInputs

unstructureSimpleTransition :: SimpleTransition -> Transition
unstructureSimpleTransition (NoSplit  wf p) = Arrow wf (unsafeWorkflowState $ S.singleton p)
unstructureSimpleTransition (ANDSplit' wf (L2 ps)) = Arrow wf (unsafeWorkflowState $ S.fromList ps)

unstructureTransition :: StructuredTransition -> [Transition]
unstructureTransition (Single tr) = [unstructureSimpleTransition tr]
unstructureTransition (XORTransitions trs) = map unstructureSimpleTransition trs

unstructureTransitions :: [StructuredTransition] -> [Transition]
unstructureTransitions = concatMap unstructureTransition

structureSimply :: [Transition] -> [SimpleTransition]
structureSimply = map mkSimpleTransition

unstructureSimply :: [SimpleTransition] -> [Transition]
unstructureSimply = map unstructureSimpleTransition
