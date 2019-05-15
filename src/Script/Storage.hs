{-|

Memory state for FCL execution.

--}

{-# LANGUAGE TupleSections #-}

module Script.Storage (
  initStorage,
  dumpStorage,
) where

import Protolude hiding ((<>))

import Ledger (World)
import Script
import Storage
import Script.Pretty
import Script.Eval (EvalM, EvalState(..), EvalCtx(..))
import qualified Script.Eval as Eval
import qualified Data.Map as Map

import Control.Monad.State.Strict (modify')

initGlobalStorage :: Script -> Storage
initGlobalStorage (Script _ defns _ _ _)
  = foldl' buildStores mempty defns
  where
    buildStores :: Storage -> Def -> Storage
    buildStores gstore = \case

      GlobalDef type_ _ (Name nm) expr ->
        Map.insert (Key nm) VUndefined gstore

      GlobalDefNull _ _ (Located _ (Name nm)) ->
        Map.insert (Key nm) VUndefined gstore

initStorage
  :: EvalCtx    -- ^ Context to evaluate the top-level definitions in
  -> World      -- ^ World to evaluate the top-level definitions in
  -> Script     -- ^ Script
  -> IO GlobalStorage
initStorage evalCtx world s@(Script _ defns _ _ _)
  = do
  res <- Eval.execEvalM evalCtx emptyEvalState $ mapM_ assignGlobal defns
  case res of
    Left err -> die $ show err
    Right state -> pure . GlobalStorage . globalStorage $ state
  where
    assignGlobal :: Def -> EvalM ()
    assignGlobal = \case
      GlobalDef type_ _ nm expr -> do
        val <- Eval.evalLExpr expr
        modify' (insertVar nm val)
      _ -> pure ()

    insertVar (Name nm) val st
      = st { globalStorage =
               Map.insert (Key nm) val (globalStorage st)
           }

    emptyEvalState :: EvalState
    emptyEvalState = EvalState
      { tempStorage      = mempty
      , globalStorage    = initGlobalStorage s
      , workflowState    = startState
      , currentMethod    = Nothing
      , worldState       = world
      , deltas           = []
      }

-- | Pretty print storage map
dumpStorage :: EnumInfo -> Map Key Value -> Doc
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
