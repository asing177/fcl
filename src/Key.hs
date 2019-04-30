module Key where

import           Protolude

class Key sk where
  sign :: sk -> ByteString -> m (Integer, Integer)
  -- verify :: pk -> sig -> ByteString -> Bool
  -- TODO: Parameterize over signature instead of (Integer, Integer)
