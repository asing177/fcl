{-# LANGUAGE DataKinds #-}
module Language.FCL.Account where

import Protolude

import qualified Language.FCL.Key as Key
import Language.FCL.Address

class Account a where
  publicKey :: a -> Key.PubKey
  accToAddress :: a -> Address AAccount
