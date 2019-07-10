{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module WorkflowGen
  ( module WorkflowGen
  ) where

import Protolude

import Data.Maybe
import Data.Set (Set(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

import Control.Monad.Gen
import Control.Monad.Writer

import Test.QuickCheck hiding (Gen)
import qualified Test.QuickCheck as QC

import Language.FCL.AST

-- NOTE: can't return back to different places from a loop (eg.: novation.s)
-- NOTE: syncronisation points are always singleton states (places) (eg.: novation.s)
-- NOTE: there is a loop hidden inside GenXOR
-- NOTE: GenXOR lhsIn lhsOut rhsIn rhsOut Nothing Nothing is equivalent to
--       XOR (Seq lhsIn lhsOut) (Seq rhsIn rhsOut)

-- | Workflow nets that are sound by construction. We only allow these _safe_ workflow nets
-- to be constructed in very specific ways in order to make soundness verification automatic.
data SafeWorkflowNet
  -- | AND splitting into multiple workflows.
  = AND { andBranches :: NonEmpty SafeWorkflowNet    -- ^ At least one branch
        }
  -- | XOR splitting into two workflows.
  | XOR { xorLhs :: SafeWorkflowNet                  -- ^ Left XOR branch
        , xorRhs :: SafeWorkflowNet                  -- ^ Right XOR branch
        }
  -- |  Sequencing two workflow.
  | Seq { seqLhs :: SafeWorkflowNet                  -- ^ First workflow
        , seqRhs :: SafeWorkflowNet                  -- ^ Second workflow
        }
  -- | Looping construct with option to exit from both the body of the loop and the head of the loop.
  -- If the first half of the body is empty, we exit from head, otherwise exit from the body.
  | GenLoop { gLoopIn   :: Maybe SafeWorkflowNet     -- ^ First half of the body of the loop
            , gLoopExit :: SafeWorkflowNet           -- ^ Exit from the loop
            , gLoopOut  :: SafeWorkflowNet           -- ^ Seconds half of the body of the loop
            }
  -- | General XOR split with the possibility of moving between the branches.
  | GenXOR { gXorLhsIn   :: SafeWorkflowNet          -- ^ First half of left-hand side
           , gXorLhsOut  :: SafeWorkflowNet          -- ^ Second half of left-hand side
           , gXorRhsIn   :: SafeWorkflowNet          -- ^ First half of right-hand side
           , gXorRhsOut  :: SafeWorkflowNet          -- ^ Second half of right-hand side
           , gXorMToRhs  :: (Maybe SafeWorkflowNet)  -- ^ Moving to the right-hand side from the left one
           , gXorMToLhs  :: (Maybe SafeWorkflowNet)  -- ^ Moving to the left-hand side from the right one
           }
  -- | Atom representing a single transition.
  | Atom
  deriving (Eq, Ord, Show, Generic)

{-# COMPLETE XOR, AND, Seq, SimpleLoop, Loop, GenXOR #-}
-- | XOR with three branches.
pattern XOR3 a b c   = XOR a (XOR b c)
-- | AND with two branches.
pattern AND2 lhs rhs = AND (lhs :| [rhs])
-- | Simple loop with exit only from the head.
pattern SimpleLoop loop   exit         = GenLoop Nothing       exit loop
-- | Loop with exit from the body.
pattern Loop       loopIn exit loopOut = GenLoop (Just loopIn) exit loopOut

-- | Make a workflow state from a `Name`.
mkWfState :: Name -> WorkflowState
mkWfState = fromPlace . makePlace

-- | Make a workflow state from a `Place`.
fromPlace :: Place -> WorkflowState
fromPlace = unsafeWorkflowState . S.singleton

-- | Generate a unique name.
genName :: (MonadGen e m, Show e) => m Name
genName = Name . show <$> gen

-- | Generate a uniq workflow state.
genWfState :: (MonadGen e m, Show e) => m WorkflowState
genWfState = mkWfState <$> genName

-- | Construct the list of transitions from a given `SafeWorkflowNet`.
constructTransitions :: SafeWorkflowNet -> [Transition]
constructTransitions = runGen . execWriterT . constructTransitionsM startState endState

-- | Construct the list of transitions from a given `SafeWorkflowNet` `swf` and `start` and `end` states.
-- The open-ended transitions of `swf` will be connected to the `start` and `end` states
-- based on the stucture of `swf`.
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

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    xs <- getNonEmpty <$> arbitrary @(QC.NonEmptyList a)
    case xs of
      (x:xs) -> return (x :| xs)
      _      -> panic "QuickCheck generated an empty list for a NonEmptyList"

  shrink (x :| xs)
    | yss <- shrink (QC.NonEmpty (x:xs))
    = (flip map) yss $ \ys ->
        case getNonEmpty ys of
          (z:zs) -> (z :| zs)
          _      -> panic "QuickCheck shrank a NonEmptyList into an empty list"

instance Arbitrary SafeWorkflowNet where
  arbitrary = sized genSWFNet where
    -- | Sized `SafeWorkflowNet generation
    genSWFNet :: Int -> QC.Gen SafeWorkflowNet
    genSWFNet 0 = pure Atom
    genSWFNet n | n > 0 = oneof
      [ AND <$> someSWFNets n
      , XOR <$> genSWFNet (n `div` 2) <*> genSWFNet (n `div` 2)
      , XOR3 <$> genSWFNet (n `div` 3) <*> genSWFNet (n `div` 3) <*> genSWFNet (n `div` 3)
      , Seq <$> genSWFNet (n `div` 2) <*> genSWFNet (n `div` 2)
      , SimpleLoop <$> genSWFNet (n `div` 2) <*> genSWFNet (n `div` 2)
      , Loop <$> genSWFNet (n `div` 3) <*> genSWFNet (n `div` 3) <*> genSWFNet (n `div` 3)
      -- crops size 3/4 of the time
      , GenXOR <$> genSWFNet (n `div` 6)
               <*> genSWFNet (n `div` 6)
               <*> genSWFNet (n `div` 6)
               <*> genSWFNet (n `div` 6)
               <*> mGenSWFNet (n `div` 6)
               <*> mGenSWFNet (n `div` 6)
      , pure Atom
      ]
    genSWFNet n = panic $ "Negative value for SafeWorkflowNet generation: " <> show n

    -- | Generates `SafeWorkflowNet`s of summed size `n`.
    someSWFNets :: Int -> QC.Gen (NonEmpty SafeWorkflowNet)
    someSWFNets n = do
      k <- frequency [ (60, pure 2)
                     , (25, pure 3)
                     , (10, pure 4)
                     , (5,  pure 5)
                     ]
      xs <- replicateM k (genSWFNet (n `div` k))
      pure $ NE.fromList xs

    -- | Sized `Maybe SafeWorkflowNet` generation
    mGenSWFNet :: Int -> QC.Gen (Maybe SafeWorkflowNet)
    mGenSWFNet n = resize n (arbitrary @(Maybe SafeWorkflowNet))

  shrink = genericShrink
