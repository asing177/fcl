module Main where
import Protolude hiding (get, from)

import HTTP.FCL.API

main :: IO ()
main = runHttpFcl
