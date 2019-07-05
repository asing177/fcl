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

-- NOTE: can't return back to different places from a loop (eg.: novation.s)
-- NOTE: syncronisation points are always singleton states (places) (eg.: novation.s)
-- NOTE: there is a loop hidden inside GenXOR
data SafeWorkflowNet
  = AND { andBranches :: NonEmpty SafeWorkflowNet }
  | XOR
  { xorLhs :: SafeWorkflowNet
  , xorRhs :: SafeWorkflowNet
  }
  | Seq
  { seqLhs :: SafeWorkflowNet
  , seqRhs :: SafeWorkflowNet
  }
  -- looping construct with option to exit from the body of the loop
  -- (can also exit from the start -- no gLoopIn)
  | GenLoop
  { gLoopIn   :: Maybe SafeWorkflowNet
  , gLoopExit :: SafeWorkflowNet
  , gLoopOut  :: SafeWorkflowNet
  }
  -- GenXOR lhsIn lhsOut rhsIn rhsOut Nothing Nothing is equivalent to
  -- XOR (Seq lhsIn lhsOut) (Seq rhsIn rhsOut)
  | GenXOR
  { gXorLhsIn   :: SafeWorkflowNet
  , gXorLhsOut  :: SafeWorkflowNet
  , gXorRhsIn   :: SafeWorkflowNet
  , gXorRhsOut  :: SafeWorkflowNet
  -- communication between the different bracnhes
  , gXorMToRhs  :: (Maybe SafeWorkflowNet)
  , gXorMToLhs  :: (Maybe SafeWorkflowNet)
  }
  | Atom
  deriving (Eq, Ord, Show)

mkWfState :: Name -> WorkflowState
mkWfState = fromPlace . makePlace

fromPlace :: Place -> WorkflowState
fromPlace = unsafeWorkflowState . S.singleton

genName :: (MonadGen e m, Show e) => m Name
genName = Name . show <$> gen

genWfState :: (MonadGen e m, Show e) => m WorkflowState
genWfState = mkWfState <$> genName

constructTransitions :: SafeWorkflowNet -> [Transition]
constructTransitions = runGen . execWriterT . constructTransitionsM startState endState

constructTransitionsM :: WorkflowState -> WorkflowState -> SafeWorkflowNet -> (WriterT [Transition] (Gen Integer)) ()
constructTransitionsM start end (AND branches) = do
  inOuts <- forM (NE.toList branches) $ \br -> do
    inSt  <- genWfState
    outSt <- genWfState
    constructTransitionsM inSt outSt br
    pure (inSt, outSt)
  let (ins, outs) = unzip inOuts
  tell [Arrow start (mconcat ins), Arrow (mconcat outs) end]
constructTransitionsM start end (XOR lhs rhs) = do
  constructTransitionsM start end lhs
  constructTransitionsM start end rhs
constructTransitionsM start end (Seq lhs rhs) = do
  inBetween <- genWfState
  constructTransitionsM start inBetween lhs
  constructTransitionsM inBetween end   rhs
constructTransitionsM start end (SimpleLoop loop exit) = do
  constructTransitionsM start start loop
  constructTransitionsM start end   exit
constructTransitionsM start end (Loop gLoopIn exit gLoopOut) = do
  inBetween <- genWfState
  constructTransitionsM start inBetween gLoopIn
  constructTransitionsM inBetween end   exit
  constructTransitionsM inBetween start gLoopOut
constructTransitionsM start end GenXOR{..} = do
  lhsP <- genWfState
  rhsP <- genWfState
  constructTransitionsM start lhsP gXorLhsIn
  constructTransitionsM lhsP  end  gXorLhsOut
  constructTransitionsM start rhsP gXorRhsIn
  constructTransitionsM rhsP  end  gXorRhsOut
  when (isJust gXorMToRhs) $
    constructTransitionsM lhsP rhsP (fromJust gXorMToRhs)
  when (isJust gXorMToLhs) $
    constructTransitionsM rhsP lhsP (fromJust gXorMToLhs)
constructTransitionsM start end Atom = do
  tell [Arrow start end]

---------------------

{-# COMPLETE XOR, AND, Seq, SimpleLoop, Loop, GenXOR #-}
pattern XOR3 a b c   = XOR a (XOR b c)
pattern AND2 lhs rhs = AND (lhs :| [rhs])
pattern SimpleLoop loop   exit         = GenLoop Nothing       exit loop
pattern Loop       loopIn exit loopOut = GenLoop (Just loopIn) exit loopOut

---------------------

namedBasicNets :: [(SafeWorkflowNet, [Char])]
namedBasicNets = zip basicNets [ "atom"
                               , "XOR"
                               , "AND"
                               , "Seq"
                               , "SimpleLoop"
                               , "Loop"
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
            , basicSimpleLoop
            , basicLoop
            , compeleteGenXOR
            , toRightGenXOR
            , toLeftGenXOR
            , simpleGenXOR
            ]

basicAtom :: SafeWorkflowNet
basicAtom = Atom

basicXOR :: SafeWorkflowNet
basicXOR = XOR Atom (XOR Atom Atom)

basicAND :: SafeWorkflowNet
basicAND = AND (Atom :| [Atom, Atom])

basicSeq :: SafeWorkflowNet
basicSeq = Seq Atom Atom

basicSimpleLoop :: SafeWorkflowNet
basicSimpleLoop = SimpleLoop Atom Atom

basicLoop :: SafeWorkflowNet
basicLoop = Loop Atom Atom Atom

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
swapNet = XOR Atom (Loop Atom Atom (XOR Atom Atom))

concurrentNet :: SafeWorkflowNet
concurrentNet = AND (Atom :| [Atom])

amendmentNet :: SafeWorkflowNet
amendmentNet = Seq (AND (Atom :| [Atom])) (SimpleLoop (XOR (Seq Atom Atom) Atom) Atom)

graphNet :: SafeWorkflowNet
graphNet = Seq (XOR (Seq Atom Atom) (Seq Atom Atom)) Atom

-- not 1:1, but kind of "isomorphic"
novationNet :: SafeWorkflowNet
novationNet = Seq (AND2 Atom Atom) (Loop (AND2 Atom Atom) Atom (AND2 Atom Atom))

loanContractNet :: SafeWorkflowNet
loanContractNet = Seq Atom (Loop Atom (XOR (Seq Atom (SimpleLoop Atom Atom)) Atom) Atom)

zcbNet :: SafeWorkflowNet
zcbNet = XOR Atom (Loop Atom (Seq Atom Atom) (XOR Atom Atom))

-- can't express with current tools: communication between XOR branches needed (discrepancy -> nomination)
-- we need GenXOR
gasForwardSimpleNet :: SafeWorkflowNet
gasForwardSimpleNet = Seq Atom (XOR (GenXOR Atom (SimpleLoop Atom Atom) Atom Atom (Just Atom) Nothing) Atom)

gasForwardNet :: SafeWorkflowNet
gasForwardNet = Seq Atom (XOR (GenXOR Atom (SimpleLoop Atom Atom) Atom gasForwardNominationSubNet (Just Atom) Nothing) Atom)

gasForwardNominationSubNet :: SafeWorkflowNet
gasForwardNominationSubNet = SimpleLoop Atom (Seq (AND2 Atom (SimpleLoop Atom Atom)) (SimpleLoop Atom Atom))

productNet :: SafeWorkflowNet
productNet = Seq Atom (XOR (Seq Atom (SimpleLoop (XOR3 Atom Atom Atom) (XOR (Seq Atom (XOR3 (Seq Atom Atom) (Seq Atom Atom) (Seq Atom Atom))) (Loop Atom (Seq Atom Atom) (XOR Atom (Seq Atom Atom)))))) Atom)
