{-|

Compiler entry points and disk serialization for scripts.

-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Script.Compile (
  -- ** Data types
  CheckedScript(..),

  -- ** Compiler
  compile,
  compilePrettyErr,
  compileFile,
  compileScript,
  compileScriptPrettyErr,
  CompilationErr(..),

  transitionSoundness,

  matchTypes,

  emptyTarget,

  -- ** Editor integration
  lintFile,
  verifyScript,
  formatScript,

  -- ** Persistence
  magicNumber,
  writeScript,
  readScript,

  -- ** Binary format
  putScript,
  getScript,

  -- ** Testing
  scriptString,
  scriptBytes,
  scriptHex,
) where

import           Protolude                  hiding (Type, TypeError)

import qualified Encoding
import qualified Storage
import qualified Utils

import           Control.Monad              (fail)
import           Control.Monad.Trans.Except (catchE, except)
import qualified Data.Aeson                 as A
import           Data.Bifunctor             (first)
import qualified Data.ByteString            as BS
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.Map                   as Map
import           Data.Serialize             as S
import qualified Data.Set                   as Set (fromList, toList)
import qualified Hexdump

import           Script                     (Name (..), Script, Transition,
                                             Type, scriptTransitions)
import qualified Script
import qualified Script.Analysis            as Analysis
import qualified Script.Duplicate           as Dupl
import qualified Script.Effect              as Effect
import qualified Script.Parser              as Parser
import           Script.Pretty              (Pretty (..), intersperse, ppr,
                                             vsep, (<+>), (<>))
import qualified Script.Pretty              as Pretty
import qualified Script.ReachabilityGraph   as Reachability
import           Script.Typecheck           (Sig, TypeError)
import qualified Script.Typecheck           as Typecheck
import qualified Script.Undefinedness       as Undef
import           Script.Warning             (Warning (..))
import           Utils                      ((?))

-------------------------------------------------------------------------------
-- FCL Compilation
-------------------------------------------------------------------------------

data CompilationErr as ac c
  = ParseErr Parser.ParseErrInfo
  | DuplicationErr [Dupl.DuplicateError as ac c]
  | TypecheckErr (NonEmpty (TypeError as ac c))
  | TransitionErr Analysis.TransitionErrors
  | WorkflowErr [Reachability.WFError]
  | UndefinednessErr [Undef.InvalidStackTrace]
  | EffectErr [Effect.EffectError as ac c]
  deriving (Generic, A.ToJSON, A.FromJSON)

instance (Ord as, Ord ac, Ord c, Pretty as, Pretty ac, Pretty c) =>
  Pretty (CompilationErr as ac c) where
  ppr (ParseErr err)         = ppr err
  ppr (DuplicationErr err)   = ppr err
  ppr (TransitionErr err)    = ppr err
  ppr (TypecheckErr err)     = ppr err
  ppr (WorkflowErr err)      = ppr err
  ppr (UndefinednessErr err) = ppr err
  ppr (EffectErr err)        = ppr err

data Pass
  = Parse
  | InferTransitions
  | WFSoundness
  | Typecheck
  | DuplCheck
  | UndefCheck
  | EffectCheck
  deriving (Eq, Show)

-- | A stage in the compiler.
stage :: Pretty.Pretty err => Pass -> Either err a -> Either Text a
stage pass = either (Left . Pretty.prettyPrint) Right

ppCompilationErr :: Pretty.Pretty err => ExceptT err (Reader env) a -> ExceptT Text (Reader env) a
ppCompilationErr ex = catchE ex (\e -> throwE (Pretty.prettyPrint e))

-- | A script after it has been checked. If it didn't have transitions, these
-- will have been filled in. Also, we get any warnings and effects that have
-- been generated/inferred during checking.
data CheckedScript as ac c = CheckedScript
  { checkedScript         :: Script as ac c
  , checkedScriptWarnings :: [Warning]
  , checkedScriptSigs     :: [(Name,Sig,Effect.Effects)]
  } deriving (Generic, A.ToJSON, A.FromJSON)

instance (Pretty as, Pretty ac, Pretty c) => Pretty (CheckedScript as ac c) where
  ppr (CheckedScript _ warns sigs) = (vsep . intersperse " ")
      [ not (null warns) ? ppr warns
      , not (null sigs) ? pprEffectSigs sigs
      ]
    where
      pprEffectSigs sigs = vsep
        $ "Method arguments and effects:" : map pprEffectSig sigs
      pprEffectSig (nm,sig,eff)
        = ppr nm <> ":" <+> ppr (sig, eff)

-- | Compile a given file into a CheckedScript (Right) or return
-- an error message (Left).
compileFile
  :: (Show as, Show ac, Show c, Ord as, Ord ac, Ord c, Pretty as, Pretty ac, Pretty c)
  => FilePath -> IO (ExceptT Text (Reader (Parser.AddrParsers as ac c)) (CheckedScript as ac c))
compileFile fpath = do
  res <- Utils.safeRead fpath
  case res of
    Left err       -> pure $ except $ Left err
    Right contents -> pure $ compilePrettyErr $ decodeUtf8 contents

-- | Compile a text stream into a CheckedScript or return an error message.
compile
  :: (Show as, Show ac, Show c, Ord as, Ord ac, Ord c)
  => Text
  -> ExceptT (CompilationErr as ac c) (Reader (Parser.AddrParsers as ac c)) (CheckedScript as ac c)
compile body = do
  addrParsers <- ask
  past <- except $ first ParseErr (Parser.parseScript addrParsers body)
  compileScript past

compilePrettyErr
  :: (Show as, Show ac, Show c, Ord as, Ord ac, Ord c, Pretty as, Pretty ac, Pretty c)
  => Text -> ExceptT Text (Reader (Parser.AddrParsers as ac c)) (CheckedScript as ac c)
compilePrettyErr = ppCompilationErr . compile

-- | Compile a Script into a CheckedScript or return an error message.
compileScript
  :: (Show as, Show ac, Show c, Ord as, Ord ac, Ord c)
  => Script as ac c
  -> ExceptT (CompilationErr as ac c) (Reader (Parser.AddrParsers as ac c)) (CheckedScript as ac c)
compileScript ast = do
    _       <- except $ first DuplicationErr (Dupl.duplicateCheck ast)
    sigs    <- except $ first TypecheckErr (Typecheck.signatures ast)
    (updatedAst, _inferredWarns) <- except $ first TransitionErr (Analysis.checkInferTransitions ast)
    graph   <- except $ first WorkflowErr (wfsoundness updatedAst)
    traces  <- except $ first UndefinednessErr (Undef.undefinednessAnalysis updatedAst)
    effects <- except $ first EffectErr (Effect.effectCheckScript updatedAst)
    let sigsEffects = Effect.combineSigsEffects sigs effects
        warnings = Undef.unusedVars traces
    pure (CheckedScript updatedAst warnings sigsEffects)
  where
    wfsoundness = Reachability.checkTransitions . Set.fromList . scriptTransitions

compileScriptPrettyErr
  :: (Show as, Show ac, Show c, Ord as, Ord ac, Ord c, Pretty as, Pretty ac, Pretty c)
  => Script as ac c -> ExceptT Text (Reader (Parser.AddrParsers as ac c)) (CheckedScript as ac c)
compileScriptPrettyErr = ppCompilationErr . compileScript

-- | Given a list of transitions, return any workflow errors
transitionSoundness :: [Transition] -> [Reachability.WFError]
transitionSoundness = Set.toList . fst . Reachability.reachabilityGraph . Set.fromList

-- | Given a file path, make sure the script parses, returning any parser errors
lintFile
  :: (Ord as, Ord ac, Ord c)
  => FilePath -> ReaderT (Parser.AddrParsers as ac c) IO [Parser.ParseErrInfo]
lintFile fpath = do
  addrParsers <- ask
  fcontents <- liftIO $ readFile fpath
  let contents = Parser.parseScript addrParsers fcontents
  case contents of
    Left err  -> pure [err]
    Right ast -> pure []

-- | Verify that a given script passes all checks succesfully.
verifyScript
  :: (Show as, Show ac, Show c, Ord as, Ord ac, Ord c)
  => Parser.AddrParsers as ac c -> Script as ac c -> Bool
verifyScript addrParsers script = isRight $ runReader (runExceptT (compileScript script)) addrParsers

-- | Compile a file pretty printing the resulting AST.
formatScript
  :: (Show as, Show ac, Show c, Ord as, Ord ac, Ord c, Pretty as, Pretty ac, Pretty c)
  => FilePath -> ExceptT Text (ReaderT (Parser.AddrParsers as ac c) IO) LText
-- IO (Either Text LText)
formatScript fpath = do
  addrParsers <- ask
  body <- liftIO $ readFile fpath
  let res = Parser.parseScript addrParsers body
  case res of
    Left err  -> except $ Left (Pretty.prettyPrint err)
    Right ast -> except $ Right (Pretty.print ast)

matchTypes :: Map.Map Name Type -> Map.Map Name Type -> Map.Map Name (Type, Type)
matchTypes a b  = Map.mapMaybe identity $ Map.intersectionWith matchTypes' a b
  where
    matchTypes' Script.TInt Script.TFloat = Nothing
    matchTypes' a b = if a == b then Nothing else Just (a,b)

-- | Empty compiler artifact
emptyTarget :: IO (Either Text ([(Name,Sig,Effect.Effects)], Script as ac c))
emptyTarget = pure (Right ([], Script.emptyScript))

-------------------------------------------------------------------------------
-- Binary Serialization
-------------------------------------------------------------------------------

{-

+-------------------+
| Header            |
+-------------------+
|                   |
| Storage           |
|                   |
+-------------------+
|                   |
| Script            |
|                   |
+-------------------+

-}

magicNumber :: ByteString
magicNumber = BS.pack [46, 70, 85, 78, 67]

maxStorage :: Int16
maxStorage = maxBound

-- | Serialize a script to disk.
putScript
  :: (Ord as, Ord ac, Ord c, S.Serialize as, S.Serialize ac, S.Serialize c)
  => Script as ac c
  -> Maybe (Storage.Storage as ac c) -> c -> PutM ()
putScript script store addr = do
  -- Header
  S.putByteString magicNumber

  -- Storage
  case store of
    (Just store) -> do
      let values = encode store
      let len = fromIntegral (BS.length values)
      putWord16be len
      S.putByteString values
    Nothing -> putWord16be 0

  -- Address
  S.put addr

   -- Script
  S.put $ Encoding.encodeBase64 (encode script)

getScript
  :: (Ord as, Ord ac, Ord c, S.Serialize as, S.Serialize ac, S.Serialize c)
  => Get (Script as ac c, Maybe (Storage.Storage as ac c), c)
getScript = do
  -- Storage
  storeLen <- fromIntegral <$> getWord16be
  storage <-
    if storeLen == 0
       then return Nothing
       else do
         sto <- decode <$> getByteString storeLen
         case sto of
           Left err -> fail "Could not decode storage."
           Right s  -> return $ Just s

  -- Address
  addr <- S.get

  -- Script
  scriptBS <- Encoding.decodeBase <$> (S.get :: Get Encoding.Base64ByteString)
  case S.decode (Encoding.unbase scriptBS) of
    Left err -> fail "Could not decode script."
    Right script ->
      pure (script, storage, addr)

-------------------------------------------------------------------------------
-- IO Operations
-------------------------------------------------------------------------------

-- | Read a script from disk.
readScript
  :: (Ord as, Ord ac, Ord c, S.Serialize as, S.Serialize ac, S.Serialize c)
  => ByteString -> Either [Char] (Script as ac c, Maybe (Storage.Storage as ac c), c)
readScript s = case BS.splitAt (BS.length magicNumber) s of
  (header, contents) ->
    if header == magicNumber
      then runGet getScript contents
      else Left "Header does not match"

-- | Write a script to disk.
writeScript
  :: (Ord as, Ord ac, Ord c, S.Serialize as, S.Serialize ac, S.Serialize c)
  => Script as ac c -> Maybe (Storage.Storage as ac c) -> c -> ByteString
writeScript script store addr = snd (runPutM (putScript script store addr))

-------------------------------------------------------------------------------
-- testing
-------------------------------------------------------------------------------

scriptString
  :: (Ord as, Ord ac, Ord c, S.Serialize as, S.Serialize ac, S.Serialize c)
  =>Script as ac c -> ByteString
scriptString s = magicNumber <> encode s

scriptBytes
  :: (Ord as, Ord ac, Ord c, S.Serialize as, S.Serialize ac, S.Serialize c)
  => Script as ac c -> [Word8]
scriptBytes s = Utils.toByteList (magicNumber <> encode s)

scriptHex
  :: (Ord as, Ord ac, Ord c, S.Serialize as, S.Serialize ac, S.Serialize c)
  => Script as ac c -> [Char]
scriptHex s = Hexdump.prettyHex (magicNumber <> encode s)
