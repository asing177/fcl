{-|

Check whether a script does not have any duplicate definitions.

-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.FCL.Duplicate
  ( duplicateCheck
  , DuplicateError(..)
  , VarSrc(..)
  ) where

import Protolude hiding ((<>))

import Data.Aeson as A
import qualified Data.Map as Map

import Language.FCL.AST
import Language.FCL.Pretty
import Language.FCL.Utils (duplicates, duplicatesOn)

-- | Duplicate variable occurrence can be in either a top level definition or
-- an argument to a method/helper function.
data VarSrc = Defn | MethodArg Name LExpr
  deriving (Show, Generic)

instance ToJSON VarSrc where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON VarSrc where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Ways we can have duplicate definitions of various kinds.
data DuplicateError
  = DuplicateMethod LName LExpr
  | DuplicateFunction LName
  | DuplicateConstructor ADTConstr
  | DuplicateField LName
  | DuplicateADTDef LNameUpper
  | DuplicateVariable VarSrc VarSrc LName
  | DuplicateTransition Transition
  | DuplicatePrecondition (Precondition, LExpr)
  deriving (Show, Generic)

instance ToJSON DuplicateError where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON DuplicateError where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Pretty DuplicateError where
  ppr (DuplicateMethod lname lexpr)
    = "Duplicate method:" <+> ppr (locVal lname) <+> "at" <+> ppr (located lname)
  ppr (DuplicateFunction lname)
    = "Duplicate helper function:" <+> ppr (locVal lname) <+> "at" <+> ppr (located lname)
  ppr (DuplicateConstructor ADTConstr{ adtConstrId })
    = "Duplicate constructor:" <+> ppr adtConstrId <+> "at" <+> ppr (located adtConstrId)
  ppr (DuplicateField f)
    = "Duplicate field" <+> sqppr f <+> "at" <+> ppr (located f)
  ppr (DuplicateADTDef lname)
    = "Duplicate adt:" <+> ppr (locVal lname) <+> "at" <+> ppr (located lname)
  ppr (DuplicateVariable varA varB lnm)
    = case (varA, varB) of
        (_, Defn) ->
          "Duplicate variable in top level variable definitions:" <+> ppr (locVal lnm) <+> "at" <+> ppr (located lnm)
        (Defn, MethodArg mnmA mnmB) ->
          sep ["The top level variable"
              , squotes (ppr (locVal lnm))
              , "is shadowed by an argument in method"
              , squotes (ppr $ mnmA) <+> "at" <+> ppr (located lnm) <> "."
              ]
        (MethodArg mnmA mnmB, MethodArg _ _) ->
          "Duplicate method argument name" <+> squotes (ppr (locVal lnm))
            <+> "in method" <+> squotes (ppr mnmA)
            <+> "at" <+> ppr (located lnm) <> "."

  ppr (DuplicateTransition t)
    = "Duplicate transition:" <+> ppr t
  ppr (DuplicatePrecondition (pr,lexpr))
    = "Duplicate precondition" <+> squotes (ppr pr) <+> "at" <+> ppr (located lexpr)

instance Pretty [DuplicateError] where
  ppr errs
    = case errs of
        []
          -> "No duplicates found"
        errs@(_:_)
          -> vsep
             $ "Duplicate errors:"
             : map (("•" <+>) . ppr) errs

duplicateCheck :: Script -> Either [DuplicateError] Script
duplicateCheck scr@(Script adts defns transitions methods helpers)
  = case allErrs of
      [] -> Right scr
      errs -> Left errs
    where
      allErrs
        = concat
          [ adtDefErrs
          , adtConstrErrs
          , adtFieldErrs
          , transErrs
          , defnAndMethodArgErrs defns methods
          , methErrs
          , helperErrs
          , methPreconditionErrs
          , defnPreconditionErrs
          ]

      adtDefErrs
        = map DuplicateADTDef
          . duplicatesOn locVal
          . map adtName
          $ adts

      adtConstrErrs
        = map DuplicateConstructor
          . duplicatesOn adtConstrId
          . concatMap adtConstrs
          $ adts

      adtFieldErrs
        = map DuplicateField
          . concat
          . (concatMap (map (duplicatesOn locVal)))
          . map (map (map snd . adtConstrParams))
          . map adtConstrs
          $ adts

      transErrs
        = map DuplicateTransition
          . duplicates
          $ transitions

      methErrs
        = map (\(name, body) -> DuplicateMethod name body)
          . duplicatesOn fst . map (\m -> (methodName m, methodBody m))
          $ methods

      helperErrs
        = map DuplicateFunction
          . duplicatesOn locVal
          . map helperName
          $ helpers

      methPreconditionErrs
        = concatMap (duplPreconditions . methodPreconditions) methods

      defnPreconditionErrs
        = concatMap duplPreconditions . mapMaybe defnPreconditions $ defns

      duplPreconditions
        = map DuplicatePrecondition
          . duplicatesOn fst
          . unPreconditions


-- | Duplicate variable checks for top level definitions and overlap with method
-- arguments. Currently, shadowing of a top level variable with a method
-- argument name is disallowed.
defnAndMethodArgErrs :: [Def] -> [Method] -> [DuplicateError]
defnAndMethodArgErrs defns methods = concatMap defnAndArgErrs methods
  where
    defnVars          = (,Defn) . defnLName <$> defns
    methArgVars m     = (\arg -> (argName arg, MethodArg (locVal $ methodName m) (methodBody m))) <$> methodArgs m

    defnAndArgErrs m  =
      snd (foldl checkDup (mempty, mempty) (defnVars ++ methArgVars m))

    checkDup (varMap, dupErrs) (lnm, src) =
      case Map.lookup (locVal lnm) varMap of
        Nothing     -> (Map.insert (locVal lnm) src varMap, dupErrs)
        Just dupSrc -> let dupErr = DuplicateVariable dupSrc src lnm
                        in (varMap, dupErrs ++ [dupErr])
