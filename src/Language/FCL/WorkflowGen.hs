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

{-# COMPLETE XOR, AND, Seq, SimpleLoop, Loop, GenXOR #-}
pattern XOR3 a b c   = XOR a (XOR b c)
pattern AND2 lhs rhs = AND (lhs :| [rhs])
pattern SimpleLoop loop   exit         = GenLoop Nothing       exit loop
pattern Loop       loopIn exit loopOut = GenLoop (Just loopIn) exit loopOut

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
