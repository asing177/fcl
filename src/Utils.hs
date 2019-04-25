{-# LANGUAGE ScopedTypeVariables #-}

{-|

Utility functions.

-}

module Utils (
  toByteList,
  showHex,
  ppShow,
  ppDump,
  ppDumpM,

  withColor,
  putRed,
  putGreen,
  putWarning,
  dieRed,

  zipWith3M,
  zipWith3M_,

  safeRead,
  safeReadLazy,
  safeWrite,
  safeWithFile,

  toInt,
  toInt64,
  toWord16,
  toWord64,

  duplicates,
  duplicatesBy,
  duplicatesOn,

  -- Error handling
  panicImpossible,

  getBinaryViaSerialize,
  putBinaryViaSerialize,

  -- * Map operations
  traverseWithKey',

  -- * Monoidal combinator
  (?),
) where

import           Protolude

import           Control.Monad        (fail)

import qualified Data.Binary          as B
import qualified Data.Binary.Get      as B
import qualified Data.Binary.Put      as B
import qualified Data.ByteArray       as BA
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List            (partition)
import qualified Data.List            as List
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Serialize       as S

import           Text.Printf          (printf)
import qualified Text.Show.Pretty     (ppShow)

import           System.Console.ANSI
import           System.Directory

-- import qualified Time

showHex :: Word8 -> [Char]
showHex = printf "%02x"

toByteList :: ByteString -> [Word8]
toByteList = BA.unpack

ppShow :: Show a => a -> Text
ppShow s = toS (Text.Show.Pretty.ppShow s)

-- | Pretty printer for showable
ppDump :: Show a => a -> IO ()
ppDump x = putStrLn (ppShow x)

-- | Pretty printer for monadic showable
--
-- > Example: ppDumpM testConfig
ppDumpM :: Show a => IO a -> IO ()
ppDumpM m = m >>= \x -> putStrLn (ppShow x)

--------------------------------------------------------------------------------

clearColor :: IO ()
clearColor = setSGR []

setColor :: Color -> IO ()
setColor color = setSGR [SetColor Foreground Vivid color]

withColor :: Color -> (a -> IO ()) -> a -> IO ()
withColor color f a =
  setColor color >> f a >> clearColor

-- | Print red text
putRed :: Text -> IO ()
putRed = withColor Red putStrLn

-- | Print green text
putGreen :: Text -> IO ()
putGreen = withColor Green putStrLn

putWarning :: Text -> IO ()
putWarning = withColor Yellow putStrLn

-- | Half exit with faillure and message.
dieRed :: Text -> IO a
dieRed msg = do
  putRed msg
  exitFailure

--------------------------------------------------------------------------------

zipWith3M :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M _ [] _ _ = pure []
zipWith3M _ _ [] _ = pure []
zipWith3M _ _ _ [] = pure []
zipWith3M f (a:as) (b:bs) (c:cs) =
  (:) <$> f a b c <*> zipWith3M f as bs cs

zipWith3M_ :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
zipWith3M_ f as bs cs = void (zipWith3M f as bs cs)

--------------------------------------------------------------------------------

{-# INLINE toInt #-}
toInt :: Integral a => a -> Int
toInt = fromInteger . toInteger

{-# INLINE toInt64 #-}
toInt64 :: Integral a => a -> Int64
toInt64 = fromInteger . toInteger

{-# INLINE toWord16 #-}
toWord16 :: Integral a => a -> Word16
toWord16 = fromInteger . toInteger

{-# INLINE toWord64 #-}
toWord64 :: Integral a => a -> Word64
toWord64 = fromInteger . toInteger

-- | Safely read a files contents, failing in a Either
safeRead :: FilePath -> IO (Either Text ByteString)
safeRead fpath = do
  exists <- doesFileExist fpath
  if exists
    then do
      contents <- BS.readFile fpath
      pure $ Right contents
    else do
      pure $ Left $ "File does not exist: " <> (show fpath)

safeReadLazy :: FilePath -> IO (Either Text BSL.ByteString)
safeReadLazy fpath = do
  exists <- doesFileExist fpath
  if exists
    then do
      contents <- BSL.readFile fpath
      pure $ Right contents
    else do
      pure $ Left $ "File does not exist: " <> (show fpath)

-- | Checks if the file exists before attempting open it.
safeWithFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO (Either Text r)
safeWithFile fpath mode f = do
  exists <- doesFileExist fpath
  if exists
     then Right <$> withFile fpath mode f
     else pure $ Left $ "File does not exist: " <> (show fpath)

safeWrite :: FilePath -> ByteString -> IO (Either Text ())
safeWrite fpath bs =
    fmap (first show) $ try' $ BS.writeFile fpath bs
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = Protolude.try

-- | Calculate the median of list of sortable elements
median :: (Fractional a, Ord a) => [a] -> Maybe a
median xs
  | n < 1 = Nothing
  | even n = Just ((nth xs (div n 2) + nth xs (div n 2 - 1)) / 2.0)
  | otherwise = Just (nth xs (div n 2))
  where
    n = length xs

nth :: Ord t => [t] -> Int -> t
nth (x:xs) n
  | k == n = x
  | k > n = nth ys n
  | otherwise = nth zs $ n - k - 1
  where
    (ys, zs) = partition (< x) xs
    k = length ys
nth [] _ = panic "Cannot index"

-- | Return a list of duplicates in a list. (Resulting list only has
-- one occurrence of every item that has duplicates.)
duplicates :: Eq a => [a] -> [a]
duplicates xs = List.nub (xs List.\\ List.nub xs)

-- | Like 'duplicates', but takes an equality relation.
duplicatesBy :: (a -> a -> Bool) -> [a] -> [a]
duplicatesBy f xs = List.nubBy f (List.deleteFirstsBy f xs (List.nubBy f xs))

duplicatesOn :: Eq b => (a -> b) -> [a] -> [a]
duplicatesOn f = duplicatesBy ((==) `on` f)

-------------------------------------------------------------------------------
-- Binary instance via Serialize
-------------------------------------------------------------------------------

getBinaryViaSerialize :: S.Serialize a => B.Get a
getBinaryViaSerialize = do
  len <- fromIntegral <$> B.getWord16be
  bs <- B.getByteString len
  case S.decode bs of
    Left err -> fail $ show err
    Right v  -> pure v

putBinaryViaSerialize :: S.Serialize a => a -> B.Put
putBinaryViaSerialize v = do
  let bs = S.encode v
  B.putWord16be $ fromIntegral (BS.length bs)
  B.putByteString bs

-------------------------------------------------------------------------------
-- Impossible Errors
-------------------------------------------------------------------------------

panicImpossible :: Maybe Text -> a
panicImpossible mText = panic $
  (<>) "The impossible happened" $
    case mText of
      Nothing  -> "!"
      Just txt -> ": " <> txt <> "!"


-------------------------------------------------------------------------------
-- * Map operations
-------------------------------------------------------------------------------

-- | Apply an effectful function to both the keys and values of a map
traverseWithKey'
  :: (Monad m, Ord k')
  => (k -> v -> m (k', v'))
  -> Map k v
  -> m (Map k' v')
traverseWithKey' f m = do
    kvs <- traverse (uncurry f) (Map.toList m)
    pure (Map.fromList kvs)

-------------------------------------------------------------------------------
-- * Monoidal Combinators
-------------------------------------------------------------------------------

-- | Return the rhs if the lhs condition is fulfilled, else return 'mempty'
-- >>> let foo n = mconcat [show n, " apple", n /= 1 ? "s"]
-- >>> foo 1
-- "1 apple"
-- >>> foo 2
-- "2 apples"
(?) :: Monoid m => Bool -> m -> m
condition ? x = if condition then x else mempty
infix 1 ?
