module Asset where

import           Protolude

data Holder ac c
  = AccountHolder ac
  | ContractHolder c
