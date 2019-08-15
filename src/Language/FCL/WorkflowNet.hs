{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.FCL.WorkflowNet (
  Token,
  Marking,
  WorkflowState,
  Transition(..),
  WorkflowNet(..),
  ColorTransition(..),
  createWorkflowNet,
  enabledTransitions,
  fireUnsafe,
) where

import Protolude

import Algebra.Lattice (JoinSemiLattice(..), (\/), joins1)

import Language.FCL.AST hiding (Transition, WorkflowState)

import Data.Map (insertWith)
import qualified Data.Map as Map
import Data.Set hiding (foldl, fold, map)
import qualified Data.Set as Set

-- | A Token is a mark to be assigned to a single Place. Tokens may or may not
-- have values assigned to them.
type Token a = a

-- | A Marking is a mapping of places to tokens, designating a _state_ of the
-- workflow net.
type Marking a = Map Place (Token a)

-- | A WorkflowState is a set of currently activated places.
type WorkflowState = Set Place

-- | A Transition is a transformation of a set of input places to a set of
-- output places. Each of the set of input places must be marked by a token in
-- order for the transactioin to be "enabled". When a transaction is enabled,
-- it can "fire" by combining all the token values assigned to its input places
-- and applying the transition function to the result of this combination. The
-- result of this function application yields a token value, one of which to be
-- placed on each of the output places of the transition. The resulting state of
-- the workflow net after firing a transition is calculuated by removing the
-- input places from the initial state and adding the resulting marking of
-- places to the state of the workflow net.
data Transition a = Transition
  { transition   :: a -> a
  , methodName   :: Name
  , inputPlaces  :: Set Place
  , outputPlaces :: Set Place
  }

-- | A representation of all WFN transitions in an adjacency matrix:
--
-- In order for methods to be callable, they must be "enabled" by a set input
-- places. If this set of input places is a subset of the current WFN state,
-- then the method is callable.
--
-- However, due to branching logic, a call of a method may transition to
-- different sets of output places depending on the current internal state of
-- the contract. Furthermore, these code branches may or may not yield a
-- different transformation of the data assigned to each input place, such that
-- each set of potential output places must be associated with a transformation
-- of the values of the input places.
--
-- Hence, a method name and a set of enabling input places serves as the keys in
-- this transition map, and a map of sets of output places to lists of
-- transformation functions. The reason for the list of transformations is such
-- that two branches in the method may transition to the same set of output
-- places, but transform the data from the input places in different ways.
type TransitionMap a =
       Map (Name, Set Place)
           (Map (Set Place) [(a -> a)])

-- | A function that takes an FCL method and an input function to serve as the
-- base with which to build the transition function that will take, as input,
-- the 'join' of all input tokens on the input places of the method. It returns
-- the mapping of potential sets of output places with their transformation
-- functions representing the different transformations and output markings that
-- may occur when branching logic occurs in the body of the method.
data ColorTransition a where
  ColorTransition
    :: JoinSemiLattice a
    => (Method -> (a -> a) -> Map WorkflowState [(a -> a)])
    -> ColorTransition a

-- | For Colored Workflow nets the 'a' represents the color. For non colored
-- workflow nets, the 'a' can simply be '()', and the transition functions can
-- all be 'identity'.
data WorkflowNet a = WorkflowNet
  { transitions :: TransitionMap a
  }

-- | Creates a workflownet from a script. It infers the input places for each
-- transition (method) in the script, then applies an abstract function to the method
-- to determine the dataflow for each output state. The dataflow is represented by
-- state-transofrming functions (possibly multiple due branching inside the method).
-- The workflownet is just a data structure that contains these dataflow functions
-- for each method.
createWorkflowNet
  :: Script            -- ^ Initial Script
  -> a                 -- ^ Input token mark of the initial state
  -> ColorTransition a -- ^ Generate a set of coloring functions from a method
  -> WorkflowNet a
createWorkflowNet s im ct =
  let initWorkflowNet = WorkflowNet mempty
   in foldl (insertMethodTransitions im ct) initWorkflowNet (scriptMethods s)

-- | Insert a method's transitions into the Workflow Net representation
insertMethodTransitions
  :: forall a. a       -- ^ Input token mark of the initial state
  -> ColorTransition a -- ^ The way to generate a mapping of output places to transformations
  -> WorkflowNet a     -- ^ The Workflow Net to insert the transition into
  -> Method            -- ^ The method being inserted
  -> WorkflowNet a
insertMethodTransitions initial (ColorTransition colorizer) wfn method =
    WorkflowNet $
      insertWith
        (Map.unionWith (++))
        (methodName, inputPlaces)
        outputPlaces
        (transitions wfn)
  where
    methodName :: Name
    methodName = locVal $ Language.FCL.AST.methodName method

    inputPlaces :: Set Place
    inputPlaces = places . methodInputPlaces $ method

    -- | Calculates the dataflow functions for the output states.
    outputPlaces :: Map (Set Place) [(a -> a)]
    -- QUESTION: can this be changed to identity? (all the tests PASS)
    -- ANSWER: `initial \/` means that the state cannot get any worse than the original state.  <-- invariant
    --         `identity` would probably do as well, because in `fireUnsafe`
    --         we are applying the function to the join of input results,
    --         and since the results are always joined, the above invariant will always hold.
    -- QUESTION: Should the invariant hold for every type of analysis or just for undefinedness?
    -- ANSWER: It probably should due the monotonicity of dataflow analyses.
    outputPlaces = colorizer method (initial \/)

-- | A transition fires iff:
--     - The input places are a subset of the current WFN marking
fire
  :: JoinSemiLattice a
  => Marking a
  -> Transition a
  -> Maybe (Marking a)
fire marking t@(Transition _ _ inputPlaces outputPlaces)
  | inputPlaces `isSubsetOf` Map.keysSet marking =
      Just (fireUnsafe marking t)
  | otherwise = Nothing

-- | Warning: does not check if transition is enabled or not
fireUnsafe
  :: forall a. JoinSemiLattice a
  => Marking a
  -> Transition a
  -> Marking a
fireUnsafe marking (Transition f _ inputs outputs) =
    -- NOTE: these are disjoint maps --> \/ is just a union
    Map.fromSet outputToken outputs \/ remainingMarkings
  where
    remainingMarkings :: Marking a
    remainingMarkings = marking `Map.withoutKeys` inputs

    -- The 'inputEnv' is the AND Join in a WFN
    --
    -- Combine all the input token values with the meet operation
    -- Warning: This function throws an exception if the input places are not
    -- a subset of the current marking. This should be checked as a
    -- precondition, before calling this function.
    inputEnv :: Token a
    inputEnv =
        case Map.elems (marking `Map.restrictKeys` inputs) of
          []   -> panic "WorkflowNet is not sound"
          t:ts -> joins1 (t :| ts)

    -- Combine the joins of the input token values with the transformation
    -- function derived from the method body
    outputToken :: Place -> Token a
    outputToken = const (f inputEnv)

-- | Fires all the enabled transitions independently,
-- and returns the resulting markings.
fireEnabledTransitions
  :: (Ord a, JoinSemiLattice a)
  => Marking a
  -> WorkflowNet a
  -> Set (Marking a)
fireEnabledTransitions marking wfn =
  Set.fromList $ map (fireUnsafe marking) (enabledTransitions marking wfn)

-- Returns the list of enabled transitions given a current WFN and a marking
enabledTransitions :: forall a. Marking a -> WorkflowNet a -> [Transition a]
enabledTransitions marking wfn =
    getTransitions enabledTransitionsMap
  where
    markedPlaces :: Set Place
    markedPlaces = Map.keysSet marking

    -- Filter the WorkflowNet map for enabled transitions by keys that are a
    -- subset of the current marking of the workflow net.
    enabledTransitionsMap :: TransitionMap a
    enabledTransitionsMap = Map.filterWithKey (checkSubset markedPlaces) (transitions wfn)

    -- Check if a transition's input places are a subset of the current marking
    checkSubset :: WorkflowState -> (Name, Set Place) -> Map (Set Place) [(a -> a)] -> Bool
    checkSubset currentState (_, inputPlaces) _ =
      inputPlaces `isSubsetOf` currentState

getTransitions :: TransitionMap a -> [Transition a]
getTransitions tmap = Map.foldMapWithKey toTransitions tmap
  where
    -- Build the list of enabled transitions from the WorkflowNet Map/Set representation
    toTransitions :: (Name, Set Place) -> Map (Set Place) [(a -> a)] -> [Transition a]
    toTransitions (name, inputPlaces) outputPlacesMap =
      flip Map.foldMapWithKey outputPlacesMap $ \outPlaces fs ->
        flip map fs $ \f -> Transition f name inputPlaces outPlaces
