{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns    #-}
module Language.FCL.SafeWorkflow.Simple
  ( SimpleSafeWorkflow

  , pattern SAtom
  , pattern SAND
  , pattern SAND2
  , pattern SGenXOR

  , gXorLhsIn
  , gXorLhsOut
  , gXorLhsToRhs
  , gXorRhsIn
  , gXorRhsOut

  , mkSimpleACF

  , module Language.FCL.SafeWorkflow
  ) where

import Protolude

import qualified Data.Map as M
import Data.List.List2

import Language.FCL.SafeWorkflow

type SimpleANDBranch    = ANDBranch    () ()
type SimpleSafeWorkflow = SafeWorkflow () ()

pattern SAtom :: SimpleSafeWorkflow
pattern SAtom = Atom ()

pattern SAND :: List2 SimpleANDBranch -> SimpleSafeWorkflow
pattern SAND branches = AND () () branches

-- | Unannotated AND with two branches.
pattern SAND2 :: SimpleSafeWorkflow -> SimpleSafeWorkflow -> SimpleSafeWorkflow
pattern SAND2 lhs rhs <- SAND (AsList [ANDBranch _ _ lhs, ANDBranch _ _ rhs])
  where SAND2 lhs rhs =  SAND [ANDBranch () () lhs, ANDBranch () () rhs]

mkSimpleACF :: ACFMap () b -> SafeWorkflow () b
mkSimpleACF acfMap = unsafeMkACF triviallyAnnotatedPlaces acfMap where
  triviallyAnnotatedPlaces :: PlaceAnnotMap ()
  triviallyAnnotatedPlaces = M.fromSet (const ()) $ collectPlaceIds acfMap

-- | Unannotated XOR with unidirectional communication between branches.
pattern SGenXOR :: SimpleSafeWorkflow -> SimpleSafeWorkflow -> SimpleSafeWorkflow -> SimpleSafeWorkflow -> SimpleSafeWorkflow -> SimpleSafeWorkflow
pattern SGenXOR lhsIn lhsOut rhsIn rhsOut lhsToRhs <- ACF _ (M.toList -> [(ACFArrow Entry (P 1), [lhsIn]), (ACFArrow (P 1) Exit, [lhsOut]), (ACFArrow Entry (P 2), [rhsIn]), (ACFArrow (P 2) Exit, [rhsOut]), (ACFArrow (P 1) (P 2), [lhsToRhs])])
  where SGenXOR lhsIn lhsOut rhsIn rhsOut lhsToRhs =  mkSimpleACF (M.fromList  [(ACFArrow Entry (P 1), [lhsIn]), (ACFArrow (P 1) Exit, [lhsOut]), (ACFArrow Entry (P 2), [rhsIn]), (ACFArrow (P 2) Exit, [rhsOut]), (ACFArrow (P 1) (P 2), [lhsToRhs])])

gXorLhsIn :: SimpleSafeWorkflow -> SimpleSafeWorkflow
gXorLhsIn (SGenXOR lhsIn _ _ _ _) = lhsIn
gXorLhsIn _= panic $ noMatchError "gXorLhsIn"

gXorLhsOut :: SimpleSafeWorkflow -> SimpleSafeWorkflow
gXorLhsOut (SGenXOR _ lhsOut _ _ _) = lhsOut
gXorLhsOut _ = panic $ noMatchError "gXorLhsOut"

gXorRhsIn :: SimpleSafeWorkflow -> SimpleSafeWorkflow
gXorRhsIn (SGenXOR _ _ rhsIn _ _) = rhsIn
gXorRhsIn _ = panic $ noMatchError "gXorRhsIn"

gXorRhsOut :: SimpleSafeWorkflow -> SimpleSafeWorkflow
gXorRhsOut (SGenXOR _ _ _ rhsOut _) = rhsOut
gXorRhsOut _ = panic $ noMatchError "gXorRhsOut"

gXorLhsToRhs :: SimpleSafeWorkflow -> SimpleSafeWorkflow
gXorLhsToRhs (SGenXOR _ _ _ _ lhsToRhs) = lhsToRhs
gXorLhsToRhs _ = panic $ noMatchError "gXorLhsToRhs"
