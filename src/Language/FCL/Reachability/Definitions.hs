{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.FCL.Reachability.Definitions where

import Protolude

import Control.Monad.RWS.Strict (RWS)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Aeson as A

import Language.FCL.AST (Place(..), Transition(..), WorkflowState(..))
import Language.FCL.Pretty (Doc, Pretty(..), (<+>), (</+>), listOf, squotes, text, vcat, setOf, (<$$>), indent, bracketList, comma)
import Language.FCL.Debug (Debug(..))

-- | A reason why the workflow is unsound. (Refer to 'Pretty' instance for
-- explanations.
data WFError
  = NotOneBounded WorkflowState Transition (Set Place)
  | ImproperCompletion WorkflowState Transition WorkflowState
  | TransFromEnd Transition
  | Counreachable WorkflowState
  | UnreachableTransition Transition
  | FreeChoiceViolation Transition Transition (Set Place)
  | NotOneBoundedMerge WorkflowState WorkflowState (Set Place)
  | ImproperCompletionMerge WorkflowState WorkflowState
  | LoopingANDBranch WorkflowState WorkflowState
  | ANDBranchGlobalExit WorkflowState
  | DirectANDSplitBranch WorkflowState WorkflowState
  deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSON WFError where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON WFError where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- TODO: separate general errors from split-and-merge errors
instance Pretty WFError where
  ppr = \case
      NotOneBounded curr t badGuys
        -> "One-boundedness violation. Applying transition" <+> qp t <+> "to state" <+> qp curr
        <+> "would result in an illegal state where the following"
        <+> case S.toList badGuys of
          [badGuy] -> "place occurs more than once:" <+> qp badGuy <> "."
          badGuys@(_:_) -> "places occur more than once:" <+> (squotes . listOf) badGuys <> "."
          [] -> panic "Error created in error."
      ImproperCompletion curr t newState
        -> "Improper completion. Applying" <+> qp t <+> "to state" <+> qp curr
        <+> "would result in the new state" <+> qp newState
        <+> "where the" <+> ppr PlaceEnd
        <+> "place is reached while other places are still active."
      TransFromEnd t
        -> "Transition from end:" <+> qp t <> "."
      Counreachable st
        -> "Dead-end state:" <+> qp st <> "."
      UnreachableTransition t
        -> "Unreachable:" <+> qp t <> "."
      FreeChoiceViolation t1 t2 shared ->
        "The workflow does not represent a free choice net:"
        <$$+> qp t1 <+> "and" <+> qp t2 <+> "share some input places:" </+> qSetOf shared
      NotOneBoundedMerge r1 r2 shared ->
        "One-boundedness violation at merge-site." </+> "When merging the results of two AND branches, more specifically" <+>
        qp r1 <+> "and" <+> qp r2 <> comma </+>
        "the resulting state contained some places more than once:" <+> qSetOf shared
      ImproperCompletionMerge r1 r2 ->
        "Improper completion at merge-site." </+> "When merging the results of two AND branches, more specifically" <+>
        qp r1 <+> "and" <+> qp r2 <> comma </+>
        "the resulting state contained the terminal place while containing other places as well."
      LoopingANDBranch splittingPoint splitHead ->
        "Erroneous looping AND branch." <+> "When AND-splitting from" <+> qp splittingPoint <> comma </+>
        "the result branch of" <+> qp splitHead <+> "loops indefinetely locally, or loops back to the splitting point."
      ANDBranchGlobalExit wfSt ->
        "An AND-branch can incorrectly loop back to the state " <+> qp wfSt <+> "." </+>
        "This kind of transition is not allowed during split-and-merge analysis."
      DirectANDSplitBranch splittingPoint splitHead ->
        "When AND-splitting from the state " <+> qp splittingPoint <> comma </+>
        "the branch of" <+> qp splitHead <+> "is a direct transition to a state" </+>
        "where no further transitions can be applied." </+>
        "This kind of transition is not allowed during split-and-merge analysis."

    where
      qp :: Pretty (Debug a) => a -> Doc
      qp = squotes . ppr . Debug

      qSetOf :: (Ord a, Pretty (Debug a)) => Set a -> Doc
      qSetOf = squotes . setOf . S.map Debug

      -- different from Language.FCL.Pretty (only indents by 2)
      (<$$+>) :: Doc -> Doc -> Doc
      (<$$+>) lhs rhs = lhs <$$> (indent 2 rhs)

instance Pretty [WFError] where
  ppr [] = "The workflow is sound."
  ppr errs@(_:_)
    = vcat . ("Workflow soundness errors:" :) $ map (("•" <+>) . ppr) errs

instance Pretty (Set WFError) where
  ppr = ppr . S.toList

-- | A Workflow reachability graph: maps a state to its immediate neighbours.
type ReachabilityGraph = Map WorkflowState (Set WorkflowState)

-- | Look up outgoing transitions from a place. E.g. for `{a, b} -> c` we would
-- map `a` and `b` to `{a, b} -> c`.
type OutgoingTransitions = Map Place (Set Transition)

-- | Pretty-print a reachability graph.
pprReachabilityGraph :: ReachabilityGraph -> Doc
pprReachabilityGraph
  = vcat
  . (text "Reachability Graph:" :)
  . map (\(k, vs) -> ppr (Debug k) <+> text "->" <+> (bracketList . map (ppr . Debug) . S.toList) vs)
  . M.toList

-- | Reachability graph builder monad with parameterizable state
type GraphBuilderM s = RWS
  OutgoingTransitions           -- R: outgoing transitions for each place in the workflow
  (Set WFError, Set Transition) -- W: errors and used transitions
  s                             -- S: the state for building the reachability graph
