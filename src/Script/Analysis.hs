{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Script.Analysis where

import Protolude hiding ((<>))

import Data.Set (Set)
import qualified Data.Aeson as A
import qualified Data.Set as Set

import Script
import Script.Pretty
import qualified Script.Prim as Prim (PrimOp (Terminate, TransitionTo, Stay))

data TransitionError
  = UndeclaredTransition Name Transition
  | UnusedTransition Transition
  deriving (Generic, A.ToJSON, A.FromJSON)

instance Pretty TransitionError where
  ppr = \case
    UndeclaredTransition name t
      -> "Undeclared:" <+> squotes (ppr t)
      <> ", implemented by method" <+> squotes (ppr name) <> "."
    UnusedTransition t
      -> "Unused:" <+> squotes (ppr t)
      <> ", which is declared but not implemented by any method."

data TransitionErrors = TransitionErrors [Transition] [TransitionError]
  deriving (Generic, A.ToJSON, A.FromJSON)

instance Pretty TransitionErrors where
  ppr = \case
    TransitionErrors _ [] -> "The declared and actual transitions match up."
    TransitionErrors inferred errs@(_:_)
      -> (vsep
        $ "The declared and actual transitions do not match up:"
        : map (("•" <+>) . ppr) errs)
        <$$> "\nInferred Transitions:\n"
        <$$> ppr inferred

-- | Infer a Script's transitions and if any transitions are declared expicitly,
-- check that they are equal, otherwise just return the script with the inferred
-- transitions.
checkInferTransitions :: Script -> Either TransitionErrors (Script, [Transition])
-- Empty transitions case: just return the script with the inferred transitions
checkInferTransitions ast@(Script _ _ [] _ _)
  = Right (ast{ scriptTransitions = ts }, ts)
    where ts = inferTransitions ast

checkInferTransitions ast@(Script _ _ trans methods _)
  = case errors of
      [] -> Right (ast, [])
      (_:_) -> Left (TransitionErrors (inferTransitions ast) errors)

  where

    actual :: [(Method, Transition)]
    actual = actualTransitions methods

    errors, undeclaredErrs, unusedErrs :: [TransitionError]
    errors = mconcat [undeclaredErrs, unusedErrs]

    unusedErrs
        = map UnusedTransition
        . Set.toList
        $ foldr Set.delete (Set.fromList trans) (map snd actual)

    undeclaredErrs = foldr step [] actual
      where
        step (method, transition) acc
          | transition `elem` trans = acc
          | otherwise = UndeclaredTransition (locVal $ methodName method) transition : acc

inferTransitions :: Script -> [Transition]
inferTransitions
    = Set.toList
    . Set.fromList
    . map snd
    . actualTransitions
    . scriptMethods

actualTransitions :: [Method] -> [(Method, Transition)]
actualTransitions methods = do
    (method, dsts) <- branches methods
    dst <- Set.toList dsts
    pure $ (method, (Script.Arrow (methodInputPlaces method) dst))
  where
    -- Fish out the actual transitions, as we get them from methods and
    -- their "transitionTo" statements, grouped by method name and source
    -- state.
    branches :: [Method] -> [(Method, Set.Set Script.WorkflowState)]
    branches = fmap extractBranch
      where
        extractBranch :: Method -> (Method, Set Script.WorkflowState)
        extractBranch bm@Method{..}
          = ( bm
            , branchesMethod bm
            )

        branchesMethod :: Method -> Set Script.WorkflowState
        branchesMethod method = Set.unions . fmap getWFState . Script.unseq . methodBody $ method
          where
            getWFState :: Script.LExpr -> Set Script.WorkflowState
            getWFState (Script.Located _ (Script.ECall (Left Prim.TransitionTo) args))
              = Set.fromList . fmap unwrap . Script.argLits . fmap Script.unLoc $ args
            getWFState (Script.Located _ (Script.ECall (Left Prim.Terminate) args))
              = Set.singleton Script.endState
            getWFState (Located _ (ECall (Left Prim.Stay) []))
              = Set.singleton (methodInputPlaces method)
            getWFState _ = mempty

            -- Safe only if run on typechecked programs.
            unwrap :: Script.LLit -> Script.WorkflowState
            unwrap (Script.Located _ (Script.LState st)) = st
            unwrap _ = panic "Malformed program."



