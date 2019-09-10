{-# LANGUAGE TemplateHaskell #-}
module Examples.SafeWorkflow.TH where

import Protolude
import NeatInterpolation

import Data.Text

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Language.FCL.AST (Expr, unLoc)
import Language.FCL.Pretty (prettyPrint)

import qualified Language.FCL.Parser as P

fcl :: QuasiQuoter
fcl = text { quoteExp = applyParseBlock . quoteExp text }

applyParseBlock :: Q Exp -> Q Exp
applyParseBlock q = appE [|parseBlock|] q

parseBlock :: Text -> Expr
parseBlock = either (panic . prettyPrint) unLoc . P.parseBlock
