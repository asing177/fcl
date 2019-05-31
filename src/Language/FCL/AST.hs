{- |

Core AST for the FCL core language.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Language.FCL.AST (
  -- ** Syntax
  Script(..),
  Expr(..),
  Pattern(..),
  Match(..),
  Method(..),
  Preconditions(..),
  Precondition(..),
  Helper(..),
  EnumDef(..),
  Def(..),
  defnPreconditions,
  Arg(..),
  Lit(..),
  BinOp(..),
  UnOp(..),

  -- ** Location info
  Loc(..),
  Located(..),
  LExpr,
  LBinOp,
  LUnOp,
  LLit,
  LName,
  LType,
  LEnumConstr,
  LPattern,

  -- ** Values
  Value(..),
  evalLit,
  evalLLit,

  -- ** Name
  Name(..),
  defnName,
  defnLName,

  -- ** Enum constructor
  EnumConstr(..),

  -- ** State Labels
  Place(..),
  Transition(..),
  WorkflowState,
  places,
  unsafeWorkflowState,
  makePlace,
  makeWorkflowState,
  startState,
  endState,
  isSubWorkflow,
  (\\),
  wfUnion,
  wfIntersection,

  -- ** DateTime
  DateTime(..),
  TimeDelta(..),

  -- ** Types
  TVar(..),
  TCollection(..),
  Type(..),

  -- ** Non-lossy Numbers
  NumPrecision(..),
  nPInt,
  normaliseNumPrecision,

  -- ** Helpers
  EnumInfo(..),
  createEnumInfo,

  eseq,
  unseq,
  argtys,
  argtys',
  argLits,
  unLoc,
  at,
  methodNames,
  lookupMethod,
  mapType,
  emptyScript,

  -- ** Pretty Printing
  ppScript,

) where

import Protolude hiding (put, get, (<>), show, Show, putByteString, Type)
import Prelude (show, Show(..))

import Control.Monad (fail)

import Numeric.Lossless.Number
import Language.FCL.Pretty
import Language.FCL.Prim (PrimOp)
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Token as Token
import qualified Language.FCL.Hash as Hash
import qualified Data.Text as T
import qualified Datetime.Types as DT

import Data.Aeson (ToJSON(..), FromJSON(..), FromJSONKey(..), ToJSONKey(..))
import qualified Data.Binary as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Data.String (IsString(..))
import Data.Serialize (Serialize(..), putInt8, getInt8)
import Data.Serialize.Text()

import Language.FCL.Address
import Language.FCL.Utils (duplicates)
import Language.FCL.SafeString
import Language.FCL.SafeInteger

-------------------------------------------------------------------------------
-- Core Language
-------------------------------------------------------------------------------

data Loc
  = NoLoc
  | Loc { line :: Int, col :: Int }
  deriving (Eq, Show, Ord, Generic, Serialize, FromJSON, ToJSON, Hash.Hashable)

data Located a = Located
  { located :: Loc
  , locVal  :: a
  } deriving (Generic, Show, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hash.Hashable)

instance Functor Located where
  fmap f (Located l v) = Located l (f v)

instance Eq a => Eq (Located a) where
  (==) l1 l2 = locVal l1 == locVal l2

instance Ord a => Ord (Located a) where
  compare l1 l2 = locVal l1 `compare` locVal l2

-- | Attach location information to a type
type LExpr = Located Expr
type LLit  = Located Lit
type LType = Located Type
type LName = Located Name
type LBinOp = Located BinOp
type LUnOp  = Located UnOp
type LEnumConstr = Located EnumConstr
type LPattern = Located Pattern

-- | Enum constructor.
newtype EnumConstr = EnumConstr { unEnumConstr :: SafeString }
  deriving (Eq, Show, Ord, Generic, Hash.Hashable)

instance ToJSON EnumConstr where
  toJSON = toJSON . unEnumConstr

instance FromJSON EnumConstr where
  parseJSON = fmap EnumConstr . parseJSON

-- | Variable names
newtype Name = Name { unName :: Text }
  deriving (Eq, Show, Ord, Generic, B.Binary, Serialize, FromJSONKey, ToJSONKey, Hash.Hashable)

-- | Datetime literals
newtype DateTime = DateTime { unDateTime :: DT.Datetime }
  deriving (Eq, Ord, Show, Generic, Serialize)

newtype TimeDelta = TimeDelta { unTimeDelta :: DT.Delta }
   deriving (Eq, Ord, Show, Generic, Serialize, ToJSON, FromJSON)

instance Hash.Hashable TimeDelta where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

instance Hash.Hashable DateTime where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

data Pattern
  = PatLit EnumConstr
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

data Match
  = Match { matchPat :: LPattern
          , matchBody :: LExpr
          }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

data Expr
  = ESeq     LExpr LExpr           -- ^ Sequencing
  | ELit     LLit                  -- ^ Literal
  | EVar     LName                 -- ^ Variable reference
  | EHole                          -- ^ An unfinished program
  | EBinOp   LBinOp LExpr LExpr    -- ^ Basic Binary ops on Integers
  | EUnOp    LUnOp  LExpr          -- ^ Basic unary ops
  | EIf      LExpr LExpr LExpr     -- ^ Conditional
  | EBefore  LExpr LExpr           -- ^ Time guard
  | EAfter   LExpr LExpr           -- ^ Time guard
  | EBetween LExpr LExpr LExpr     -- ^ Time guard
  | ECase    LExpr [Match]         -- ^ Case statement
  | EAssign  Name  LExpr           -- ^ Variable update
  | ECall    (Either PrimOp LName) [LExpr] -- ^ Function call
  | ENoOp                          -- ^ Empty method body
  | EMap     (Map LExpr LExpr)     -- ^ Map k v
  | ESet     (Set LExpr)           -- ^ Set v
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

data BinOp
  = Add     -- ^ Addition
  | Sub     -- ^ Subtraction
  | Mul     -- ^ Multiplication
  | Div     -- ^ Division
  | And     -- ^ Logical conjunction
  | Or      -- ^ Logical disjunction
  | Equal   -- ^ Equality
  | NEqual  -- ^ Unequality
  | LEqual  -- ^ Lesser equal
  | GEqual  -- ^ Greater equal
  | Lesser  -- ^ Lesser
  | Greater -- ^ Greater
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

data UnOp = Not -- ^ Logical negation
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

-- | Literal representing Value
data Lit
  = LNum       Decimal
  | LBool      Bool
  | LState     WorkflowState
  | LAccount   (Address AAccount)
  | LAsset     (Address AAsset)
  | LContract  (Address AContract)
  | LText       SafeString
  | LSig       (SafeInteger, SafeInteger)
  | LDateTime  DateTime
  | LTimeDelta TimeDelta
  | LConstr    EnumConstr
  | LVoid
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

-- | Values in which literals are evaluated to
data Value
  = VNum Number                    -- ^ Number
  | VBool Bool                     -- ^ Boolean value
  | VAccount (Address AAccount)    -- ^ Account Address
  | VAsset (Address AAsset)        -- ^ Asset Address
  | VContract (Address AContract)  -- ^ Contract Address
  | VText SafeString                -- ^ Msgs (ASCII)
  | VSig (SafeInteger, SafeInteger) -- ^ ESDSA Sig
  | VVoid                          -- ^ Void
  | VDateTime DateTime             -- ^ A datetime with a timezone
  | VTimeDelta TimeDelta           -- ^ A difference in time
  | VEnum EnumConstr               -- ^ Constructor of the given enum type
  | VState WorkflowState           -- ^ Named state label
  | VMap (Map Value Value)         -- ^ Map of values to values
  | VSet (Set Value)               -- ^ Set of values
  | VUndefined                     -- ^ Undefined
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable)

-- | Type variables used in inference
data TVar
  = TV  Text -- ^ General type variable
  | TAV Text -- ^ Type variable used for inferring return types of prim ops operating over assets
  | TCV Text -- ^ Type variable used for inferring return types of prim ops operating over collections
  | THV Type -- ^ Type variable used for inferring holdings type of polymorphic asset prim ops.
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

data TCollection
  = TMap Type Type  -- ^ Type of FCL Maps
  | TSet Type       -- ^ Type of FCL Sets
  deriving (Eq, Ord, Show, Generic, Serialize, FromJSON, ToJSON, Hash.Hashable)

-- | The required numeric precision @p@ to ensure non-lossy arithmetic. This is
-- tracked in the type of numbers, @TNum p@.
data NumPrecision
  = NPArbitrary                      -- ^ arbitrary precision
  | NPDecimalPlaces Integer          -- ^ fixed number of decimal places
  | NPAdd NumPrecision NumPrecision  -- ^ internal (for multiplication)
  | NPMax NumPrecision NumPrecision  -- ^ internal (for addition)
  deriving (Eq, Ord, Show, Generic, Serialize, FromJSON, ToJSON, Hash.Hashable)

-- | Smart constructor for 'NumPrecision' for integers
nPInt :: NumPrecision
nPInt = NPDecimalPlaces 0

normaliseNumPrecision :: NumPrecision -> NumPrecision
normaliseNumPrecision = go
  where
    go = \case
      NPAdd (go -> NPDecimalPlaces p1) (go -> NPDecimalPlaces p2)  -> NPDecimalPlaces $ p1 + p2
      NPAdd (go -> NPArbitrary) _                  -> NPArbitrary
      NPAdd _                  (go -> NPArbitrary) -> NPArbitrary
      NPMax (go -> NPDecimalPlaces p1) (go -> NPDecimalPlaces p2)  -> NPDecimalPlaces $ p1 `max` p2
      NPMax (go -> NPArbitrary) _                  -> NPArbitrary
      NPMax _                  (go -> NPArbitrary) -> NPArbitrary
      npX -> npX

-- | Core types for FCL
data Type
  = TError          -- ^ (Internal) Error branch in typechecker
  | TVar TVar       -- ^ (Internal) Type variable used in inference
  | TAny            -- ^ (Internal) Polymorphic type
  | TNum NumPrecision           -- ^ Type of rational numbers
  | TBool           -- ^ Type of booleans
  | TAccount        -- ^ Type of account addresses
  | TAsset Type     -- ^ Type of asset addresses
  | TContract       -- ^ Type of contract addresses
  | TText            -- ^ Type of messages
  | TSig            -- ^ Type of ECDSA signature
  | TVoid           -- ^ Type of void
  | TDateTime       -- ^ DateTime with Timezone
  | TTimeDelta      -- ^ Type of difference in time
  | TState          -- ^ Contract state
  | TEnum Name      -- ^ Enumeration type
  | TFun [Type] Type -- ^ Type signature of helper functions--argument types and return type
  | TColl TCollection -- ^ Type of collection values
  | TTransition     -- ^ Transition type
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

-- | Function argument
data Arg
  = Arg { argType :: Type, argName ::  LName }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

data Precondition = PrecAfter | PrecBefore | PrecRoles
  deriving (Eq, Ord, Show, Generic, Serialize, FromJSON, ToJSON, Hash.Hashable)

instance Pretty Precondition where
  ppr = \case
    PrecAfter  -> "after"
    PrecBefore -> "before"
    PrecRoles  -> "roles"

newtype Preconditions = Preconditions
  { unPreconditions :: [(Precondition, LExpr)] }
  deriving (Eq, Ord, Show, Generic, Serialize, FromJSON, ToJSON, Hash.Hashable)

instance Semigroup Preconditions where
  Preconditions ps1 <> Preconditions ps2 = Preconditions $ ps1 <> ps2

instance Monoid Preconditions where
  mempty = Preconditions []

instance Pretty Preconditions where
  ppr (Preconditions ps)
    = case map prettyPrec ps of
        [] -> ""
        ps -> bracketList ps
    where
      prettyPrec = \case
        (PrecRoles, locVal -> ESet s) | length s == 1
          -> "role:" <+> ppr (Set.elemAt 0 s)
        (p, e) -> ppr p <> ":" <+> ppr e

-- | Method
data Method = Method
  { methodInputPlaces   :: WorkflowState
  , methodPreconditions :: Preconditions
  , methodName          :: LName
  , methodArgs          :: [Arg]
  , methodBody          :: LExpr
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

-- | "Pure" Helper functions
data Helper = Helper
  { helperName :: LName
  , helperArgs :: [Arg]
  , helperBody :: LExpr
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

-- | Enumeration
data EnumDef = EnumDef
  { enumName :: LName
  , enumConstrs :: [LEnumConstr]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Hash.Hashable)

-- | Definition
data Def
  = GlobalDef Type Preconditions Name LExpr
  | GlobalDefNull Type Preconditions LName
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hash.Hashable)

defnName :: Def -> Name
defnName = locVal . defnLName

defnLName :: Def -> LName
defnLName = \case
  GlobalDef _ _ nm _    -> Located NoLoc nm
  GlobalDefNull _ _ lnm -> lnm

defnPreconditions :: Def -> Maybe Preconditions
defnPreconditions = \case
    GlobalDef _ p _ _   -> Just p
    GlobalDefNull _ p _ -> Just p

-- | Script
data Script = Script
  { scriptEnums       :: [EnumDef]
  , scriptDefs        :: [Def]
  , scriptTransitions :: [Transition]
  , scriptMethods     :: [Method]
  , scriptHelpers     :: [Helper]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Hash.Hashable)

emptyScript :: Script
emptyScript = Script
  { scriptEnums       = []
  , scriptDefs        = []
  , scriptTransitions = []
  , scriptMethods     = []
  , scriptHelpers     = []
  }

-- | Method args names and types
argtys :: Method -> [(Name,Type)]
argtys m = [(unLoc lnm, ty) | Arg ty lnm <- methodArgs m ]

argtys' :: Method -> [Type]
argtys' = map snd . argtys

-- | Method argument literals
argLits :: [Expr] -> [LLit]
argLits (ELit n : xs) = n : argLits xs
argLits (_ : xs) = argLits xs
argLits [] = []

-- | Method names
methodNames :: Script -> [LName]
methodNames = fmap methodName . scriptMethods

lookupMethod :: Name -> Script -> Maybe Method
lookupMethod nm s =
  find ((==) nm . locVal . methodName) (scriptMethods s)

-- | Remove position information on an element
unLoc :: Located a -> a
unLoc (Located loc a) = a

-- | Put location information on an element
at :: a -> Loc -> Located a
at = flip Located

-- | Unroll a sequence of statements into a list.
unseq :: LExpr -> [LExpr]
unseq (Located l e) = case e of
  ESeq a b       -> unseq a ++ unseq b
  EIf cond tr fl -> unseq tr ++ unseq fl
  EBefore _ s    -> unseq s
  EAfter _ s     -> unseq s
  EBetween _ _ s -> unseq s
  ECase _ ms     -> concatMap (unseq . matchBody) ms
  _              -> [Located l e]

-- | Roll a list of expressions into a sequence.
eseq :: Loc -> [LExpr] -> LExpr
eseq loc es = case es of
  []     -> Located loc ENoOp
  [x]    -> x
  (x:xs) -> Located loc $
    ESeq x $ eseq (located x) xs

-- @Nothing@ in case of an unknown constructor.
mapType :: EnumInfo -> Value -> Maybe Type
mapType _        (VNum (NumRational _)) = pure (TNum NPArbitrary)
mapType _        (VNum (NumDecimal f))  = (pure . TNum . NPDecimalPlaces . decimalPlaces) f
mapType enumInfo (VEnum c)    = TEnum <$> Map.lookup c (constrToEnum enumInfo)
mapType _        VBool{}      = pure TBool
mapType _        VAccount{}   = pure TAccount
mapType _        VAsset{}     = pure (TAsset TAny)
mapType _        VContract{}  = pure TContract
mapType _        VVoid        = pure TVoid
mapType _        VText{}       = pure TText
mapType _        VSig{}       = pure TSig
mapType _        VDateTime{}  = pure TDateTime
mapType _        VTimeDelta{} = pure TTimeDelta
mapType _        VState{}     = pure TState
mapType _        VUndefined   = pure TAny
mapType einfo   (VMap vmap)   =
  case Map.toList vmap of
    []        -> pure (TColl (TMap TAny TAny))
    ((k,v):_) -> TColl <$> (TMap <$> mapType einfo k <*> mapType einfo v)
mapType einfo   (VSet vset)   =
  case Set.toList vset of
    []        -> pure (TColl (TSet TAny))
    (v:_) -> TColl <$> (TSet <$> mapType einfo v)

data EnumInfo = EnumInfo
  { constrToEnum :: Map EnumConstr Name
  , enumToConstrs :: Map Name [EnumConstr]
  }

-- | Create the dictionaries for the constructor/enum type membership
-- relations from the original list of definitionSet. Assumes that there
-- are no duplicates in the input.
createEnumInfo :: [EnumDef] -> EnumInfo
createEnumInfo enums = EnumInfo constrEnum enumConstrs
  where
    constrEnum
      = Map.fromList
      . concatMap (\(EnumDef lname constrs)
                     -> map (\lconstr -> (locVal lconstr, locVal lname)) constrs)
      $ enums

    enumConstrs
      = Map.fromList
      . map (\(EnumDef lname constrs)
               -> (locVal lname, map locVal constrs))
      $ enums

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance Serialize (NonEmpty LExpr) where
  put = put . NE.toList
  get = NE.fromList <$> get

instance IsString Name where
  fromString = Name . toS

instance IsString TVar where
  fromString = TV . toS

instance Serialize TVar where
  put (TV tv)  = putInt8 0 >> put (encodeUtf8 tv)
  put (TAV tv) = putInt8 1 >> put (encodeUtf8 tv)
  put (TCV tv) = putInt8 2 >> put (encodeUtf8 tv)
  put (THV tv) = putInt8 3 >> put tv
  get = do
    tag <- getInt8
    case tag of
      0 -> TV . decodeUtf8 <$> get
      1 -> TAV . decodeUtf8 <$> get
      2 -> TCV . decodeUtf8 <$> get
      3 -> THV <$> get
      n -> fail "Inavlid tag deserialized for TVar"

instance IsString EnumConstr where
  fromString = EnumConstr . fromString

instance Serialize EnumConstr where
  put = put . unEnumConstr
  get = EnumConstr <$> get

instance (Serialize a) => Serialize (Located a) where
  put (Located loc x) = put (loc,x)
  get = uncurry Located <$> get

instance Serialize Lit where
instance Serialize EnumDef where
instance Serialize Def where
instance Serialize Arg where
instance Serialize Type where
instance Serialize Pattern where
instance Serialize Match where
instance Serialize Expr where
instance Serialize BinOp where
instance Serialize UnOp where
instance Serialize Method where
instance Serialize Helper where
instance Serialize Script where

-------------------------------------------------------------------------------
-- Pretty Printer
-------------------------------------------------------------------------------

instance Pretty TimeDelta where
  ppr (TimeDelta d) = ppr $ DT.displayDelta d

instance Pretty Loc where
  ppr NoLoc     = "No location info"
  ppr (Loc l c) = "Line" <+> append "," (ppr l) <+> "Column" <+> ppr c

instance (Pretty a) => Pretty (Located a) where
  ppr (Located _ x) = ppr x

instance Pretty Name where
  ppr (Name nm) = ppr nm

instance Pretty EnumConstr where
  ppr (EnumConstr ec) = ppr ec

instance Pretty Expr where
  ppr = \case
    ENoOp            -> mempty
    ESeq e e'        -> vsep [ppr' e, ppr e']
                      where
                        ppr' (Located _ expr) =
                          case expr of
                            ENoOp     -> ppr expr
                            otherwise -> semify $ ppr expr
    EHole            -> token Token.hole
    ELit lit         -> ppr lit
    EVar nm          -> ppr nm
    EAssign nm e     -> ppr nm <+> token Token.assign <+> ppr e
    EUnOp nm e       -> parens $ ppr nm <> ppr e
    EBinOp nm e e'   -> parens $ ppr e <+> ppr nm <+> ppr e'
    ECall nm es      -> hcat [ppr nm, tupleOf (map ppr es)]
    EBefore dt e     -> token Token.before <+> parens (ppr dt) <+> lbrace
                   <$$> indent 2 ((if e' == ENoOp then identity else semify) (ppr e'))
                   <$$> rbrace
                      where
                        Located _ e' = e

    EAfter dt e      -> token Token.after <+> parens (ppr dt) <+> lbrace
                   <$$> indent 2 ((if e' == ENoOp then identity else semify) (ppr e'))
                   <$$> rbrace
                      where
                        Located _ e' = e

    ECase e ms -> token Token.case_ <> parens (ppr e) <+> lbrace
                   <$$> indent 2 (vsep (map (semify . ppr) ms))
                   <$$> rbrace

    EBetween d1 d2 e -> token Token.between <+> tupleOf (map ppr [d1,d2]) <+> lbrace
                   <$$> indent 2 ((if e' == ENoOp then identity else semify) (ppr e'))
                   <$$> rbrace

                      where
                        Located _ e' = e
    EIf c e1 e2      -> token Token.if_ <+> parens (ppr c) <+> lbrace
                   <$$> (indent 2 . if e1' == ENoOp then identity else semify) (ppr e1)
                   <$$> (if e2' == ENoOp
                           then mempty
                           else rbrace <+> token Token.else_ <+> lbrace
                          <$$> (indent 2 . semify . ppr) e2
                        )
                   <$$> rbrace
                      where
                        Located _ e1' = e1
                        Located _ e2' = e2
    EMap m -> tupleOf $ map (\(k,v) -> ppr k <+> ":" <+> ppr v) (Map.toList m)
    ESet s -> setOf s


instance Pretty Match where
  ppr (Match (Located _ (PatLit p)) expr)
    = ppr (LConstr p) <+> token Token.rarrow <+> maybeBrace expr
    where
      -- Wrap braces around the expression in case it is a sequence of
      -- statements, otherwise don't.
      maybeBrace e
        = case locVal e of
            ESeq _ _ -> lbrace <+> semify (ppr e) <+> rbrace
            _ -> ppr e

instance Pretty Lit where
  ppr = \case
    LNum n         -> ppr n
    LBool bool     -> ppr bool
    LText msg       -> dquotes $ ppr msg
    LAccount addr  -> "u" <> squotes (ppr addr)
    LAsset addr    -> "a" <> squotes (ppr addr)
    LContract addr -> "c" <> squotes (ppr addr)
    LSig (r,s)     -> tupleOf [ppr r, ppr s]
    LVoid          -> token Token.void
    LState name    -> token Token.colon <> ppr name
    LDateTime dt   -> dquotes $ ppr $ (DT.formatDatetime (unDateTime dt) :: [Char])
    LTimeDelta d   -> ppr d
    LConstr ec     -> text "`" <> ppr ec

instance Pretty Type where
  ppr = \case
    TNum p      -> case normaliseNumPrecision p of
        NPArbitrary         -> token Token.num
        (NPDecimalPlaces 0) -> token Token.int
        (NPDecimalPlaces p) -> token Token.decimal <> angles (ppr p)
        _                   -> panic $ "Expected normalised num precision"
    TBool       -> token Token.bool
    TAny        -> token Token.any
    TAsset t    -> token Token.asset <> angles (ppr t)
    TAccount    -> token Token.account
    TContract   -> token Token.contract
    TVoid       -> token Token.void
    TSig        -> token Token.sig
    TText       -> token Token.text
    TError      -> text $ "<error-type>"  -- Not in syntax
    TVar v      -> ppr v
    TDateTime   -> token Token.datetime
    TTimeDelta  -> token Token.timedelta
    TState      -> token Token.state
    TEnum e     -> token Token.enum <+> token (unName e)
    TFun as r   -> tupleOf (map ppr as) <+> "->" <+> ppr r
    TColl tcol  -> ppr tcol
    TTransition -> token Token.transition

instance Pretty TVar where
  ppr (TV v)  = text (toSL v)
  ppr (TAV v) = "asset" <> angles (text (toSL v))
  ppr (TCV v) = "coll" <> angles (text (toSL v))
  ppr (THV t) = "holdings" <> angles (ppr t)

instance Pretty TCollection where
  ppr = \case
    TMap tk tv -> token Token.map <> angles (ppr tk <> "," <+> ppr tv)
    TSet ts    -> token Token.set <> angles (ppr ts)

instance Pretty Value where
  ppr = \case
    VNum n       -> ppr n
    VBool n      -> ppr n
    VAccount n   -> ppr n
    VAsset n     -> ppr n
    VContract n  -> ppr n
    VText n       -> dquotes $ ppr n
    VSig (r,s)   -> ppr r <> "," <> ppr s
    VVoid        -> token Token.void
    VDateTime dt -> ppr (DT.formatDatetime (unDateTime dt) :: [Char])
    VTimeDelta d -> ppr d
    VEnum c      -> ppr c
    VState n     -> ppr n
    VMap vmap    -> ppr vmap
    VSet vset    -> tupleOf (Set.toList vset)
    VUndefined   -> "undefined"

instance Pretty Arg where
  ppr (Arg typ name) = ppr typ <+> ppr name

instance Pretty BinOp where
  ppr = \case
    Add     -> token Token.add
    Sub     -> token Token.sub
    Div     -> token Token.div
    Mul     -> token Token.mult
    And     -> token Token.and
    Or      -> token Token.or
    Equal   -> token Token.equal
    NEqual  -> token Token.nequal
    LEqual  -> token Token.lequal
    GEqual  -> token Token.gequal
    Lesser  -> token Token.lesser
    Greater -> token Token.greater

instance Pretty UnOp where
  ppr Not = token Token.not

instance Pretty Method where
  ppr (Method inputPs precs name args (Located _ body)) =
    token Token.at <> ppr inputPs <+> ppr precs
      <$$> ppr name <> tupleOf (map ppr args) <+>
      case body of
        ENoOp -> lbrace <+> rbrace
        e@(ESeq _ _) -> lbrace
          <$$> indent 2 (semify $ ppr e)
          <$$> rbrace
        other -> lbrace <$$> indent 2 (semify (ppr other)) <$$> rbrace

instance Pretty Helper where
  ppr (Helper name args (Located _ body)) =
    ppr name <> tupleOf (map ppr args) <+>
      case body of
        ENoOp -> lbrace <+> rbrace
        e@(ESeq _ _) -> lbrace
          <$$> indent 2 (semify $ ppr e)
          <$$> rbrace
        other -> lbrace <$$> indent 2 (semify (ppr other)) <$$> rbrace

instance Pretty EnumDef where
  ppr (EnumDef lname lconstrs)
    = token Token.enum <+> ppr (locVal lname) <+> lbrace
      <$$> (Pretty.commafy . map (ppr . locVal) $ lconstrs)
      <$$> rbrace <> token Token.semi

instance Pretty Def where
  ppr = \case
    GlobalDefNull typ precs (Located _ name)
      -> hsep [token Token.global, ppr typ, ppr precs, ppr name] <> token Token.semi
    GlobalDef typ precs name expr
      -> hsep [token Token.global, ppr typ, ppr precs, ppr name `assign` ppr expr]

instance Pretty Script where
  ppr (Script enums defns transitions methods functions) = vsep
    [ vsep (map ppr enums)
    , vsep (map ppr defns)
    , Pretty.softbreak
    , ppr transitions
    , Pretty.softbreak
    , vsep (spaced (map ppr methods))
    , Pretty.softbreak
    , vsep (spaced (map ppr functions))
    ]

ppScript :: Script -> LText
ppScript = render . ppr

-------------------------------------------------------------------------------
-- Map Literals
-------------------------------------------------------------------------------

evalLit :: Lit -> Value
evalLit lit = case lit of
  LNum n      -> VNum (NumDecimal n)
  LVoid       -> VVoid
  LBool n     -> VBool n
  LAccount n  -> VAccount n
  LAsset n    -> VAsset n
  LContract n -> VContract n
  LText n      -> VText n
  LSig n      -> VSig n
  LState pl   -> VState pl
  LDateTime d -> VDateTime d
  LTimeDelta d -> VTimeDelta d
  LConstr c   -> VEnum c

evalLLit :: LLit -> Value
evalLLit (Located _ lit) = evalLit lit

-------------------------------------------------------------------------------
-- To/FromJSON
-------------------------------------------------------------------------------

instance ToJSON Name where
  toJSON (Name nm) = toJSON nm

instance FromJSON Name where
  parseJSON = fmap Name . parseJSON

instance ToJSON DateTime where
  toJSON (DateTime dt) = toJSON dt

instance FromJSON DateTime where
  parseJSON = fmap DateTime . parseJSON

------------------------------------------------------------------------------
-- Graph analysis
-------------------------------------------------------------------------------

data Place
  = PlaceStart
  | Place { placeName :: Name }
  | PlaceEnd
  deriving (Eq, Ord, Show, Generic, Serialize, ToJSON, FromJSON, FromJSONKey, ToJSONKey, Hash.Hashable)

instance Pretty Place where
  ppr PlaceStart = ppr Token.initial
  ppr (Place id) = ppr id
  ppr PlaceEnd = ppr Token.terminal

makePlace :: Name -> Place
makePlace nm@(Name n)
  | n == Token.initial = PlaceStart
  | n == Token.terminal = PlaceEnd
  | otherwise =  Place nm

startState, endState :: WorkflowState
startState = WorkflowState $ Set.singleton PlaceStart
endState = WorkflowState $ Set.singleton PlaceEnd

newtype WorkflowState = WorkflowState { places :: Set Place }
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable)

instance Pretty WorkflowState where
  ppr (Set.toList . places -> [p]) = ppr p
  ppr (Set.toList . places -> ps) = listOf ps

instance ToJSON WorkflowState where
  toJSON = toJSON . prettyPrint

instance FromJSON WorkflowState where
  parseJSON = fmap (WorkflowState . Set.fromList . fmap (makePlace . Name) . T.splitOn (T.pack ",")) . parseJSON

data Transition
  = Arrow WorkflowState WorkflowState
  deriving (Eq, Ord, Show, Generic, Serialize, ToJSON, FromJSON, Hash.Hashable)

instance Pretty Transition where
  ppr (Arrow from to) = token Token.transition <+> ppr from <+> token Token.rarrow <+> ppr to

instance Pretty [Transition] where
  ppr [] = "/* No transitions */"
  ppr ts = vsep . map (semify . ppr) $ ts

isSubWorkflow :: WorkflowState -> WorkflowState -> Bool
isSubWorkflow (WorkflowState w1) (WorkflowState w2) = w1 `Set.isSubsetOf` w2

(\\), wfUnion, wfIntersection :: WorkflowState -> WorkflowState -> WorkflowState
((\\), wfUnion, wfIntersection)
  = (liftWF (Set.\\), liftWF Set.union, liftWF Set.intersection)
  where
    liftWF op (WorkflowState w1) (WorkflowState w2) = WorkflowState $ w1 `op` w2


makeWorkflowState :: [Name] -> Either Doc WorkflowState
makeWorkflowState names
  | null dups = Right . WorkflowState . Set.fromList $ map makePlace names
  | otherwise = Left $ "Duplicate places:" <+> (hcat . map ppr) dups
  where
    dups = duplicates names

-- | Doesn't check if workflow state is valid
unsafeWorkflowState :: Set Place -> WorkflowState
unsafeWorkflowState = WorkflowState
