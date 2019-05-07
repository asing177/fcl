{-|

Memory state for FCL execution.

--}

{-# LANGUAGE TupleSections #-}

module Script.Storage (
  initStorage,
  dumpStorage,
) where

import Protolude hiding ((<>))

import Ledger (WorldOps, Addressable)
import qualified Key
import Script
import Storage
import Script.Pretty
import Script.Eval (EvalM, EvalState(..), EvalCtx(..))
import qualified Script.Eval as Eval
import qualified Data.Map as Map

import Control.Monad.State.Strict (modify')

initGlobalStorage :: forall as ac c. Script as ac c -> Storage as ac c
initGlobalStorage (Script _ defns _ _ _)
  = foldl' buildStores mempty defns
  where
    buildStores :: Storage as ac c -> Def as ac c -> Storage as ac c
    buildStores gstore = \case

      GlobalDef type_ _ (Name nm) expr ->
        Map.insert (Key nm) VUndefined gstore

      GlobalDefNull _ _ (Located _ (Name nm)) ->
        Map.insert (Key nm) VUndefined gstore

initStorage
  :: forall as ac c asset account sk world.
  (Ord as, Ord ac, Ord c, Show as, Show ac, Show c, Ledger.Addressable asset as, Ledger.Addressable account ac, Ledger.WorldOps world, Key.Key sk)
  => Proxy (asset, account)
  -> EvalCtx as ac c sk    -- ^ Context to evaluate the top-level definitions in
  -> world      -- ^ World to evaluate the top-level definitions in
  -> Script as ac c     -- ^ Script
  -> IO (GlobalStorage as ac c)
initStorage _ evalCtx world s@(Script _ defns _ _ _)
  = do
  res <- runExceptT . Eval.execEvalM evalCtx emptyEvalState $ mapM_ assignGlobal defns
  case res of
    Left err -> die $ show err
    Right state -> pure . GlobalStorage . globalStorage $ state
  where
    assignGlobal :: Def as ac c -> (EvalM as ac c asset account sk world) ()
    assignGlobal = \case
      GlobalDef type_ _ nm expr -> do
        val <- Eval.evalLExpr expr
        modify' (insertVar nm val)
      _ -> pure ()

    insertVar (Name nm) val st
      = st { globalStorage =
               Map.insert (Key nm) val (globalStorage st)
           }

    emptyEvalState :: EvalState as ac c asset account world
    emptyEvalState = EvalState
      { tempStorage      = mempty
      , globalStorage    = initGlobalStorage s
      , workflowState    = startState
      , currentMethod    = Nothing
      , worldState       = world
      , deltas           = []
      }

-- | Pretty print storage map
dumpStorage :: (Pretty as, Pretty ac, Pretty c) => EnumInfo -> Map Key (Value as ac c) -> Doc
dumpStorage enumInfo store =
  if Map.null store
    then indent 8 "<empty>"
    else
      indent 8 $
      vcat [
          ppr k                         -- variable
          <+> ":" <+> pprTy v  -- type
          <+> "=" <+> ppr v             -- value
          | (k,v) <- Map.toList store
        ]
  where
    pprTy v = case mapType enumInfo v of
                Nothing -> "<<unknown constructor>>"
                Just ty -> ppr ty
