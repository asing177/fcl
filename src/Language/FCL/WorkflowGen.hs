{-# LANGUAGE PatternSynonyms #-}
module Language.FCL.WorkflowGen
  ( module Language.FCL.WorkflowGen
  ) where

import Protolude

import Data.Set (Set(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

import Control.Monad.Gen
import Control.Monad.Writer

import Language.FCL.AST
import Language.FCL.ReachabilityGraph

-- NOTE: no XOR split from "multiple" places
-- (since we are working with free choice nets, we can AND merge those places -- sort of)
-- NOTE: can't return back to a place that easily, only through loop
-- NOTE: can't return back to different places from a loop (novation.s)
data SafeWorkflowNet
  = XOR (NonEmpty SafeWorkflowNet)
  | AND (NonEmpty SafeWorkflowNet)
  | Seq SafeWorkflowNet SafeWorkflowNet
  -- looping workflow with option to exit from start
  -- XOR splits from the beginning
  | LoopXOR SafeWorkflowNet SafeWorkflowNet
  -- looping workflow with option to exit from start and from the body of the loop
  -- XOR splits from the body
  | LoopSeqXOR SafeWorkflowNet SafeWorkflowNet SafeWorkflowNet
  | Atom
  deriving (Eq, Ord, Show)

data IncompleteTransition
  = From WorkflowState
  | To WorkflowState
  | Empty
  deriving (Eq, Ord, Show)

incompleteFrom :: Name -> IncompleteTransition
incompleteFrom n = From (mkWfState n)

incompleteTo :: Name -> IncompleteTransition
incompleteTo n = To (mkWfState n)

mkWfState :: Name -> WorkflowState
mkWfState = fromPlace . makePlace

fromPlace :: Place -> WorkflowState
fromPlace = unsafeWorkflowState . S.singleton

mkTranstion :: Name -> Name -> Transition
mkTranstion lhs rhs = Arrow (mkWfState lhs) (mkWfState rhs)

plugTransition :: Name -> Name -> IncompleteTransition -> Transition
plugTransition _ to (From wfSt) = Arrow wfSt (mkWfState to)
plugTransition from _ (To wfSt) = Arrow (mkWfState from) wfSt
plugTransition from to Empty    = mkTranstion from to

plugFrom :: Name -> IncompleteTransition -> Either IncompleteTransition Transition
plugFrom from (To to)      = Right $ Arrow (mkWfState from) to
plugFrom from itr@(From{}) = Left itr
plugFrom from Empty        = Left $ From (mkWfState from)

plugTo :: Name -> IncompleteTransition -> Either IncompleteTransition Transition
plugTo to (From from) = Right $ Arrow (mkWfState to) from
plugTo to itr@(To{})  = Left itr
plugTo to Empty       = Left $ To (mkWfState to)

-- plugWithPlace :: Place -> IncompleteTransition -> Place -> Transition
-- plugWithPlace _ (From wfSt) p = Arrow wfSt (fromPlace p)
-- plugWithPlace p (To wfSt) _ = Arrow (fromPlace p) wfSt
-- plugWithPlace from Empty to = Arrow (fromPlace from) (fromPlace to)

plugWithState :: WorkflowState -> WorkflowState -> IncompleteTransition -> Transition
plugWithState _ to (From wfSt) = Arrow wfSt to
plugWithState from _ (To wfSt) = Arrow from wfSt
plugWithState from to Empty    = Arrow from to

genName :: (MonadGen e m, Show e) => m Name
genName = Name . show <$> gen

genWfState :: (MonadGen e m, Show e) => m WorkflowState
genWfState = mkWfState <$> genName

constructTransitions :: SafeWorkflowNet -> [Transition]
constructTransitions = runGen . execWriterT . constructTransitionsM startState endState

constructTransitionsM :: WorkflowState -> WorkflowState -> SafeWorkflowNet -> (WriterT [Transition] (Gen Integer)) ()
constructTransitionsM start end (XOR wfs) =
  mapM_ (constructTransitionsM start end) (NE.toList wfs)
constructTransitionsM start end (AND branches) = do
  inOuts <- forM (NE.toList branches) $ \br -> do
    inSt  <- genWfState
    outSt <- genWfState
    constructTransitionsM inSt outSt br
    pure (inSt, outSt)
  let (ins, outs) = unzip inOuts
  tell [Arrow start (mconcat ins), Arrow (mconcat outs) end]
constructTransitionsM start end (Seq lhs rhs) = do
  inBetween <- genWfState
  constructTransitionsM start inBetween lhs
  constructTransitionsM inBetween end   rhs
constructTransitionsM start end (LoopXOR loop exit) = do
  constructTransitionsM start start loop
  constructTransitionsM start end   exit
constructTransitionsM start end (LoopSeqXOR loopIn exit loopOut) = do
  inBetween <- genWfState
  constructTransitionsM start inBetween loopIn
  constructTransitionsM inBetween end   exit
  constructTransitionsM inBetween start loopOut
constructTransitionsM start end Atom = do
  tell [Arrow start end]

---------------------

pattern XOR2 lhs rhs = XOR (lhs :| [rhs])
pattern AND2 lhs rhs = AND (lhs :| [rhs])

---------------------

namedBasicConstruct :: [(SafeWorkflowNet, [Char])]
namedBasicConstruct = zip basicConstructs ["atom", "XOR", "AND", "Seq", "LoopXOR", "LoopSeqXor"]

basicConstructs :: [SafeWorkflowNet]
basicConstructs = [basicAtom, basicXOR, basicAND, basicSeq, basicLoopXOR, basicLoopSeqXOR]

basicAtom :: SafeWorkflowNet
basicAtom = Atom

basicXOR :: SafeWorkflowNet
basicXOR = XOR (Atom :| [Atom, Atom])

basicAND :: SafeWorkflowNet
basicAND = AND (Atom :| [Atom, Atom])

basicSeq :: SafeWorkflowNet
basicSeq = Seq Atom Atom

basicLoopXOR :: SafeWorkflowNet
basicLoopXOR = LoopXOR Atom Atom

basicLoopSeqXOR :: SafeWorkflowNet
basicLoopSeqXOR = LoopSeqXOR Atom Atom Atom

------------------------

namedExampleNets :: [(SafeWorkflowNet, [Char])]
namedExampleNets = zip exampleNets [ "swap"
                                   , "concurrent"
                                   , "amendment"
                                   , "graph"
                                   , "novation"
                                   , "loan-contract"
                                   , "zcb"
                                   ]

exampleNets :: [SafeWorkflowNet]
exampleNets = [ swapNet
              , concurrentNet
              , amendmentNet
              , graphNet
              , novationNet
              , loanContractNet
              , zcbNet
              ]

swapNet :: SafeWorkflowNet
swapNet = XOR2 Atom (LoopSeqXOR Atom Atom (XOR2 Atom Atom))

concurrentNet :: SafeWorkflowNet
concurrentNet = AND (Atom :| [Atom])

amendmentNet :: SafeWorkflowNet
amendmentNet = Seq (AND (Atom :| [Atom])) (LoopXOR (XOR2 (Seq Atom Atom) Atom) Atom)

graphNet :: SafeWorkflowNet
graphNet = Seq (XOR2 (Seq Atom Atom) (Seq Atom Atom)) Atom

-- not 1:1, but kind of "isomorphic"
novationNet :: SafeWorkflowNet
novationNet = Seq (AND2 Atom Atom) (LoopSeqXOR (AND2 Atom Atom) Atom (AND2 Atom Atom))

loanContractNet :: SafeWorkflowNet
loanContractNet = Seq Atom (LoopSeqXOR Atom (XOR2 (Seq Atom (LoopXOR Atom Atom)) Atom) Atom)

zcbNet :: SafeWorkflowNet
zcbNet = XOR2 Atom (LoopSeqXOR Atom (Seq Atom Atom) (XOR2 Atom Atom))

-- can't express with current tools: communication between XOR branches needed (discrepancy -> nomination)
gasForward :: SafeWorkflowNet
gasForward = undefined -- Seq Atom (XOR2 () Atom))
