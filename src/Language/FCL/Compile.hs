{-|

Compiler entry points and disk serialization for scripts.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.FCL.Compile (
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

import Protolude hiding (Type, TypeError)
import Test.QuickCheck
import qualified Language.FCL.Utils as Utils
import qualified Language.FCL.Storage as Storage
import Language.FCL.Address
import qualified Language.FCL.Encoding as Encoding

import Data.Bifunctor (first)
import Data.Serialize as S
import Data.List.NonEmpty (NonEmpty)
import qualified Data.ByteString as BS
import qualified Hexdump
import Data.Aeson as A hiding (encode, decode)
import qualified Data.Map as Map
import qualified Data.Set as Set (fromList, toList)
import Control.Monad (fail)

import Language.FCL.AST
import qualified Language.FCL.Analysis as Analysis
import Language.FCL.Typecheck (Sig, TypeError)
import Language.FCL.Pretty (Pretty(..), (<>), (<+>), intersperse, ppr, vsep)
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Parser as Parser
import qualified Language.FCL.Duplicate as Dupl
import qualified Language.FCL.Effect as Effect
import qualified Language.FCL.Typecheck as Typecheck
import qualified Language.FCL.Reachability.General as Reachability
import qualified Language.FCL.Undefinedness as Undef
import Language.FCL.Warning (Warning(..))
import Language.FCL.Utils ((?))

-------------------------------------------------------------------------------
-- FCL Compilation
-------------------------------------------------------------------------------

data CompilationErr
  = ParseErr Parser.ParseErrInfo
  | DuplicationErr [Dupl.DuplicateError]
  | TypecheckErr (NonEmpty TypeError)
  | TransitionErr Analysis.TransitionErrors
  | WorkflowErr [Reachability.WFError]
  | UndefinednessErr [Undef.InvalidStackTrace]
  | EffectErr [Effect.EffectError]
  deriving (Show, Generic)

instance ToJSON CompilationErr where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON CompilationErr where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Pretty CompilationErr where
  ppr (ParseErr err) = ppr err
  ppr (DuplicationErr err) = ppr err
  ppr (TransitionErr err) = ppr err
  ppr (TypecheckErr err) = ppr err
  ppr (WorkflowErr err) = ppr err
  ppr (UndefinednessErr err) = ppr err
  ppr (EffectErr err) = ppr err

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

ppCompilationErr :: Pretty.Pretty err => Either err a -> Either Text a
ppCompilationErr = either (Left . Pretty.prettyPrint) Right

-- | A script after it has been checked. If it didn't have transitions, these
-- will have been filled in. Also, we get any warnings and effects that have
-- been generated/inferred during checking.
data CheckedScript = CheckedScript
  { checkedScript         :: Script
  , checkedScriptWarnings :: [Warning]
  , checkedScriptSigs     :: [(Name,Sig,Effect.Effects)]
  } deriving (Generic)

instance ToJSON CheckedScript where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON CheckedScript where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Pretty CheckedScript where
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
compileFile :: FilePath -> IO (Either Text CheckedScript)
compileFile fpath = do
  res <- Utils.safeRead fpath
  case res of
    (Left err) -> return $ Left err
    (Right contents) -> return $ compilePrettyErr $ decodeUtf8 contents


-- | Compile a text stream into a CheckedScript or return an error message.
compile :: Text -> Either CompilationErr CheckedScript
compile body = do
  past <- first ParseErr (Parser.parseScript body)
  compileScript past

compilePrettyErr :: Text -> Either Text CheckedScript
compilePrettyErr = ppCompilationErr . compile

-- | Compile a Script into a CheckedScript or return an error message.
compileScript :: Script -> Either CompilationErr CheckedScript
compileScript ast = do
    _       <- first DuplicationErr (Dupl.duplicateCheck ast)
    sigs    <- first TypecheckErr (Typecheck.signatures ast)
    (updatedAst, _inferredWarns) <- first TransitionErr (Analysis.checkInferTransitions ast)
    graph   <- first WorkflowErr (wfsoundness updatedAst)
    traces  <- first UndefinednessErr (Undef.undefinednessAnalysis updatedAst)
    effects <- first EffectErr (Effect.effectCheckScript updatedAst)
    let sigsEffects = Effect.combineSigsEffects sigs effects
        warnings = Undef.unusedVars traces
    pure (CheckedScript updatedAst warnings sigsEffects)
  where
    wfsoundness = Reachability.checkTransitions . Set.fromList . scriptTransitions

compileScriptPrettyErr :: Script -> Either Text CheckedScript
compileScriptPrettyErr = ppCompilationErr . compileScript

-- | Given a list of transitions, return any workflow errors
transitionSoundness :: [Transition] -> [Reachability.WFError]
transitionSoundness = Set.toList . fst . Reachability.completeReachabilityGraph . Set.fromList

-- | Given a file path, make sure the script parses, returning any parser errors
lintFile :: FilePath -> IO [Parser.ParseErrInfo]
lintFile fpath = do
  fcontents <- readFile fpath
  let contents = Parser.parseScript fcontents
  case contents of
    Left err  -> pure [err]
    Right ast -> pure []

-- | Verify that a given script passes all checks succesfully.
verifyScript :: Script -> Bool
verifyScript = isRight . compileScript

-- | Compile a file pretty printing the resulting AST.
formatScript :: FilePath -> IO (Either Text LText)
formatScript fpath = do
  body <- readFile fpath
  let res = Parser.parseScript body
  case res of
    Left err  -> pure $ Left (Pretty.prettyPrint err)
    Right ast -> pure $ Right (Pretty.print ast)

matchTypes :: Map.Map Name Type -> Map.Map Name Type -> Map.Map Name (Type, Type)
matchTypes a b  = Map.mapMaybe identity $ Map.intersectionWith matchTypes' a b
  where
    matchTypes' a b = if a == b then Nothing else Just (a,b)

-- | Empty compiler artifact
emptyTarget :: IO (Either Text ([(Name,Sig,Effect.Effects)], Script))
emptyTarget = pure (Right ([], emptyScript))

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
putScript :: Script -> Maybe Storage.Storage -> Address AContract -> PutM ()
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

getScript :: Get (Script, Maybe Storage.Storage, Address AContract)
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
           Right s -> return $ Just s

  -- Address
  addr <- S.get

  -- Script
  scriptBS <- Encoding.decodeBase <$> (S.get :: Get Encoding.Base64ByteString)
  case S.decode (Encoding.unbase scriptBS) of
    Left err -> fail $ "Could not decode " <> show err
    Right script ->
      pure (script, storage, addr)

-------------------------------------------------------------------------------
-- IO Operations
-------------------------------------------------------------------------------

-- | Read a script from disk.
readScript :: ByteString -> Either [Char] (Script, Maybe Storage.Storage, Address AContract)
readScript s = case BS.splitAt (BS.length magicNumber) s of
  (header, contents) ->
    if header == magicNumber
      then runGet getScript contents
      else Left "Header does not match"

-- | Write a script to disk.
writeScript :: Script -> Maybe Storage.Storage -> Address AContract -> ByteString
writeScript script store addr = snd (runPutM (putScript script store addr))

-------------------------------------------------------------------------------
-- testing
-------------------------------------------------------------------------------

scriptString :: Script -> ByteString
scriptString s = magicNumber <> encode s

scriptBytes :: Script -> [Word8]
scriptBytes s = Utils.toByteList (magicNumber <> encode s)

scriptHex :: Script -> [Char]
scriptHex s = Hexdump.prettyHex (magicNumber <> encode s)

---------------
-- Arbitrary
---------------

instance Arbitrary CompilationErr where
  arbitrary = oneof
    [ ParseErr <$> arbitrary
    , DuplicationErr <$> arbitrary
    ]

