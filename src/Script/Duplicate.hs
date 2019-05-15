{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|

Check whether a script does not have any duplicate definitions.

(Does not really take care of shadowing.)

-}

module Script.Duplicate
  ( duplicateCheck
  , DuplicateError(..)
  , VarSrc(..)
  ) where

import Protolude hiding ((<>))

import qualified Data.Aeson as A
import qualified Data.Map as Map

import Script
import Script.Pretty
import Utils (duplicates, duplicatesOn)

-- | Duplicate variable occurrence can be in either a top level definition or
-- an argument to a method/helper function.
data VarSrc = Defn | MethodArg Name LExpr
  deriving (Generic, A.ToJSON, A.FromJSON)

-- | Ways we can have duplicate definitions of various kinds.
data DuplicateError
  = DuplicateMethod LName LExpr
  | DuplicateFunction LName
  | DuplicateConstructor LEnumConstr
  | DuplicateEnumDef LName
  | DuplicateVariable VarSrc VarSrc LName
  | DuplicateTransition Transition
  | DuplicatePrecondition (Precondition, LExpr)
  deriving (Generic, A.ToJSON, A.FromJSON)

instance Pretty DuplicateError where
  ppr (DuplicateMethod lname lexpr)
    = "Duplicate method:" <+> ppr (locVal lname) <+> "at" <+> ppr (located lname)
  ppr (DuplicateFunction lname)
    = "Duplicate helper function:" <+> ppr (locVal lname) <+> "at" <+> ppr (located lname)
  ppr (DuplicateConstructor lc)
    = "Duplicate constructor:" <+> ppr (locVal lc) <+> "at" <+> ppr (located lc)
  ppr (DuplicateEnumDef lname)
    = "Duplicate enum:" <+> ppr (locVal lname) <+> "at" <+> ppr (located lname)
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
duplicateCheck scr@(Script enums defns transitions methods helpers)
  = case allErrs of
      [] -> Right scr
      errs -> Left errs
    where
      allErrs
        = concat
          [ enumDefErrs
          , enumConstrErrs
          , transErrs
          , defnAndMethodArgErrs defns methods
          , methErrs
          , helperErrs
          , methPreconditionErrs
          , defnPreconditionErrs
          ]

      enumDefErrs
        = map DuplicateEnumDef
          . duplicatesOn locVal
          . map enumName
          $ enums

      enumConstrErrs
        = map DuplicateConstructor
          . duplicatesOn locVal
          . concatMap enumConstrs
          $ enums

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
