{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.FCL.Analysis where

import Protolude hiding ((<>))

import Data.Set (Set)
import qualified Data.Aeson as A
import qualified Data.Set as Set

import Language.FCL.AST
import Language.FCL.Pretty
import qualified Language.FCL.Prim as Prim (PrimOp (Terminate, TransitionTo, Stay))

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
    pure $ (method, (Language.FCL.Arrow (methodInputPlaces method) dst))
  where
    -- Fish out the actual transitions, as we get them from methods and
    -- their "transitionTo" statements, grouped by method name and source
    -- state.
    branches :: [Method] -> [(Method, Set.Set Language.FCL.WorkflowState)]
    branches = fmap extractBranch
      where
        extractBranch :: Method -> (Method, Set Language.FCL.WorkflowState)
        extractBranch bm@Method{..}
          = ( bm
            , branchesMethod bm
            )

        branchesMethod :: Method -> Set Language.FCL.WorkflowState
        branchesMethod method = Set.unions . fmap getWFState . Language.FCL.unseq . methodBody $ method
          where
            getWFState :: Language.FCL.LExpr -> Set Language.FCL.WorkflowState
            getWFState (Language.FCL.Located _ (Language.FCL.ECall (Left Prim.TransitionTo) args))
              = Set.fromList . fmap unwrap . Language.FCL.argLits . fmap Language.FCL.unLoc $ args
            getWFState (Language.FCL.Located _ (Language.FCL.ECall (Left Prim.Terminate) args))
              = Set.singleton Language.FCL.endState
            getWFState (Located _ (ECall (Left Prim.Stay) []))
              = Set.singleton (methodInputPlaces method)
            getWFState _ = mempty

            -- Safe only if run on typechecked programs.
            unwrap :: Language.FCL.LLit -> Language.FCL.WorkflowState
            unwrap (Language.FCL.Located _ (Language.FCL.LState st)) = st
            unwrap _ = panic "Malformed program."



