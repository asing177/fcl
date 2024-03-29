{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Key (
  keyTests,
) where

import Protolude

import Test.Tasty
import Test.QuickCheck.Monadic
import Test.Tasty.QuickCheck

import Crypto.PubKey.ECC.ECDSA (private_d)

import Language.FCL.Key
import qualified Language.FCL.Address as Address
import qualified Language.FCL.Encoding as Encoding

import Test.Reference as Reference

keyTests :: TestTree
keyTests =
  testGroup "Key Tests"
    [ localOption (QuickCheckTests 100) $
        testProperty "ECDSA key recovery" $ \(Positive k) ->
          monadicIO $ do
            let msg = "The quick brown fox ..."

            let (pub, prv) = new' k
            sig <- run $ sign prv msg
            let addr = Reference.deriveAddress pub

            let (addr1, addr2) = Reference.recoverAddress sig msg
            assert $ (addr1 == addr) || (addr2 == addr)

    , localOption (QuickCheckTests 10000) $
        testProperty "dehexPub (encodeUtf8 $ hexPub pubKey) == pubKey)" $ \pubKey ->
          let
            pubKeyHexBS = decodeUtf8 (unHexPub $ encodeHexPub pubKey)
            ePubKey = case Encoding.parseEncodedBS $ encodeUtf8 pubKeyHexBS of
              Left (Encoding.BadEncoding err) -> Left (show err)
              Right b -> decodeHexPub (HexPub b)
          in case ePubKey of
            Left err      -> False
            Right pubKey' -> pubKey == pubKey'

    , localOption (QuickCheckTests 10000) $
        testProperty "(uncompressPair . compressPair) (skScalar, pkPoint) == (skScalar, pkPoint)" $ \sk ->
          let pk = toPublic sk
              d  = private_d sk
          in (==) (d, extractPoint pk)
                  (uncompressPair . (flip compressPair d) $ extractPoint pk)

    , localOption (QuickCheckTests 10000) $
        testProperty "(uncompressPoint . compressPoint) pubKeyCoords == Right pubKeyCoords " $ \pk ->
          extractPoint pk == ((uncompressPoint . compressPoint) $ extractPoint pk)

    , localOption (QuickCheckTests 10000) $
        testProperty "(importPriv . exportPriv) sk == Right sk " $ \sk ->
          Right sk == fmap snd ((importPriv . exportPriv) sk)

    , localOption (QuickCheckTests 10000) $
        testProperty "(importPub . exportPub) pk == Right pk " $ \pk ->
          Right pk == (importPub . exportPub) pk
    ]
