{-# LANGUAGE PatternSynonyms #-}
module Language.FCL.WorkflowGen
  ( module Language.FCL.WorkflowGen
  ) where

import Protolude

import Data.Maybe
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
-- NOTE: can't return back to different places from a loop (eg.: novation.s)
-- NOTE: syncronisation points are always singleton states (places) (eg.: novation.s)
data SafeWorkflowNet
  = XOR
  { xorBranches :: NonEmpty SafeWorkflowNet
  }
  | AND
  { andBranches :: NonEmpty SafeWorkflowNet
  }
  | Seq
  { seqLhs :: SafeWorkflowNet
  , seqRhs :: SafeWorkflowNet
  }
  -- looping workflow with option to exit from start
  -- XOR splits from the beginning
  | Loop
  { loopIn   :: Maybe SafeWorkflowNet
  , loopExit :: SafeWorkflowNet
  , loopOut  :: SafeWorkflowNet
  }
  | GenXOR
  { gxorLhsIn   :: SafeWorkflowNet
  , gxorLhsOut  :: SafeWorkflowNet
  , gxorRhsIn   :: SafeWorkflowNet
  , gxorRhsOut  :: SafeWorkflowNet
  , gxorMToRhs  :: (Maybe SafeWorkflowNet)
  , gxorMToLhs  :: (Maybe SafeWorkflowNet)
  }
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
constructTransitionsM start end GenXOR{..} = do
  lhsP <- genWfState
  rhsP <- genWfState
  when (any isJust [gxorMToRhs, gxorMToLhs]) $ do
    constructTransitionsM start lhsP gxorLhsIn
    constructTransitionsM lhsP  end  gxorLhsOut
    constructTransitionsM start rhsP gxorRhsIn
    constructTransitionsM rhsP  end  gxorRhsOut
  when (isJust gxorMToRhs) $
    constructTransitionsM lhsP rhsP (fromJust gxorMToRhs)
  when (isJust gxorMToLhs) $
    constructTransitionsM rhsP lhsP (fromJust gxorMToLhs)
constructTransitionsM start end Atom = do
  tell [Arrow start end]

---------------------

{-# COMPLETE XOR, AND, Seq, LoopXOR, LoopSeqXOR, GenXOR  #-}
pattern XOR2 lhs rhs = XOR (lhs :| [rhs])
pattern XOR3 a b c   = XOR (a   :| [b,c])
pattern AND2 lhs rhs = AND (lhs :| [rhs])
pattern LoopXOR    loop   exit         = Loop Nothing       exit loop
pattern LoopSeqXOR loopIn exit loopOut = Loop (Just loopIn) exit loopOut

---------------------

namedBasicNets :: [(SafeWorkflowNet, [Char])]
namedBasicNets = zip basicNets [ "atom"
                               , "XOR"
                               , "AND"
                               , "Seq"
                               , "LoopXOR"
                               , "LoopSeqXor"
                               , "compeleteGenXOR"
                               , "toRightGenXOR"
                               , "toLeftGenXOR"
                               , "simpleGenXOR"
                               ]

basicNets :: [SafeWorkflowNet]
basicNets = [ basicAtom
            , basicXOR
            , basicAND
            , basicSeq
            , basicLoopXOR
            , basicLoopSeqXOR
            , compeleteGenXOR
            , toRightGenXOR
            , toLeftGenXOR
            , simpleGenXOR
            ]

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

compeleteGenXOR :: SafeWorkflowNet
compeleteGenXOR = GenXOR Atom Atom Atom Atom (Just Atom) (Just Atom)

toRightGenXOR :: SafeWorkflowNet
toRightGenXOR = GenXOR Atom Atom Atom Atom (Just Atom) Nothing

toLeftGenXOR :: SafeWorkflowNet
toLeftGenXOR = GenXOR Atom Atom Atom Atom Nothing (Just Atom)

simpleGenXOR :: SafeWorkflowNet
simpleGenXOR = GenXOR Atom Atom Atom Atom Nothing Nothing

------------------------

namedExampleNets :: [(SafeWorkflowNet, [Char])]
namedExampleNets = zip exampleNets [ "swap"
                                   , "concurrent"
                                   , "amendment"
                                   , "graph"
                                   , "novation"
                                   , "loan-contract"
                                   , "zcb"
                                   , "product"
                                   , "gas-forward-simple"
                                   , "gas-forward"
                                   ]

exampleNets :: [SafeWorkflowNet]
exampleNets = [ swapNet
              , concurrentNet
              , amendmentNet
              , graphNet
              , novationNet
              , loanContractNet
              , zcbNet
              , productNet
              , gasForwardSimpleNet
              , gasForwardNet
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
-- we need GenXOR
gasForwardSimpleNet :: SafeWorkflowNet
gasForwardSimpleNet = Seq Atom (XOR2 (GenXOR Atom (LoopXOR Atom Atom) Atom Atom (Just Atom) Nothing) Atom)

gasForwardNet :: SafeWorkflowNet
gasForwardNet = Seq Atom (XOR2 (GenXOR Atom (LoopXOR Atom Atom) Atom gasForwardNominationSubNet (Just Atom) Nothing) Atom)

gasForwardNominationSubNet :: SafeWorkflowNet
gasForwardNominationSubNet = LoopXOR Atom (Seq (AND2 Atom (LoopXOR Atom Atom)) (LoopXOR Atom Atom))

productNet :: SafeWorkflowNet
productNet = Seq Atom (XOR2 (Seq Atom (LoopXOR (XOR3 Atom Atom Atom) (XOR2 (Seq Atom (XOR3 (Seq Atom Atom) (Seq Atom Atom) (Seq Atom Atom))) (LoopSeqXOR Atom (Seq Atom Atom) (XOR2 Atom (Seq Atom Atom)))))) Atom)
