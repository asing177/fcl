{-

Core AST for the FCL core language.

-}

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Script (
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
  Script.Value(..),
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
  TAsset(..),
  TCollection(..),
  Type(..),

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

import           Prelude             (Show (..), show)
import           Protolude           hiding (Show, Type, get, put,
                                      putByteString, show, (<>))

import           Control.Monad       (fail)

import           Fixed
import           Script.Pretty
import qualified Script.Pretty       as Pretty
import           Script.Prim         (PrimOp)

import qualified Data.Text           as T
import qualified Datetime.Types      as DT
import qualified Hash
import           SafeInteger
import           SafeString
import qualified Script.Token        as Token

import           Data.Aeson          (FromJSON (..), FromJSONKey (..),
                                      ToJSON (..), ToJSONKey (..))
import qualified Data.Binary         as B
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map            as Map
import           Data.Serialize      (Serialize (..), getInt8, putInt8)
import           Data.Serialize.Text ()
import qualified Data.Set            as Set
import           Data.String         (IsString (..))

import           Utils               (duplicates)

-------------------------------------------------------------------------------
-- Core Language
-------------------------------------------------------------------------------

data Loc
  = NoLoc
  | Loc { line :: Int, col :: Int }
  deriving (Eq, Show, Ord, Generic, NFData, Serialize, Hash.Hashable, FromJSON, ToJSON)

data Located a = Located
  { located :: Loc
  , locVal  :: a
  } deriving (Generic, Hash.Hashable, Show, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

instance Functor Located where
  fmap f (Located l v) = Located l (f v)

instance NFData a => NFData (Located a)

instance Eq a => Eq (Located a) where
  (==) l1 l2 = locVal l1 == locVal l2

instance Ord a => Ord (Located a) where
  compare l1 l2 = locVal l1 `compare` locVal l2

-- | Attach location information to a type
type LExpr ac as c = Located (Expr ac as c)
type LLit ac as c = Located (Lit ac as c)
type LType = Located Type
type LName = Located Name
type LBinOp = Located BinOp
type LUnOp  = Located UnOp
type LEnumConstr = Located EnumConstr
type LPattern = Located Pattern

-- | Enum constructor.
newtype EnumConstr = EnumConstr { unEnumConstr :: SafeString }
  deriving (Eq, Show, Ord, Generic, Hash.Hashable, NFData)

instance ToJSON EnumConstr where
  toJSON = toJSON . unEnumConstr

instance FromJSON EnumConstr where
  parseJSON = fmap EnumConstr . parseJSON

-- | Variable names
newtype Name = Name { unName :: Text }
  deriving (Eq, Show, Ord, Generic, Hash.Hashable, NFData, B.Binary, Serialize, FromJSONKey, ToJSONKey)

-- | Datetime literals
newtype DateTime = DateTime { unDateTime :: DT.Datetime }
  deriving (Eq, Ord, Show, Generic, NFData, Serialize)

newtype TimeDelta = TimeDelta { unTimeDelta :: DT.Delta }
   deriving (Eq, Ord, Show, Generic, NFData, Serialize, ToJSON, FromJSON)

instance Hash.Hashable TimeDelta where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

instance Hash.Hashable DateTime where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

data Pattern
  = PatLit EnumConstr
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData, FromJSON, ToJSON, Serialize)

data Match ac as c
  = Match { matchPat  :: LPattern
          , matchBody :: LExpr ac as c
          }
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData, FromJSON, ToJSON, Serialize)

data Expr ac as c
  = ESeq     (LExpr ac as c) (LExpr ac as c)           -- ^ Sequencing
  | ELit     (LLit ac as c)                  -- ^ Literal
  | EVar     LName                 -- ^ Variable reference
  | EHole
  | EBinOp   LBinOp (LExpr ac as c) (LExpr ac as c)    -- ^ Basic Binary ops on Integers
  | EUnOp    LUnOp  (LExpr ac as c)          -- ^ Basic unary ops
  | EIf      (LExpr ac as c) (LExpr ac as c) (LExpr ac as c)     -- ^ Conditional
  | EBefore  (LExpr ac as c) (LExpr ac as c)           -- ^ Time guard
  | EAfter   (LExpr ac as c) (LExpr ac as c)           -- ^ Time guard
  | EBetween (LExpr ac as c) (LExpr ac as c) (LExpr ac as c)     -- ^ Time guard
  | ECase    (LExpr ac as c) [Match ac as c]         -- ^ Case statement
  | EAssign  Name  (LExpr ac as c)           -- ^ Variable update
  | ECall    (Either PrimOp LName) [LExpr ac as c] -- ^ Function call
  | ENoOp                          -- ^ Empty method body
  | EMap     (Map (LExpr ac as c) (LExpr ac as c))     -- ^ Map k v
  | ESet     (Set (LExpr ac as c))           -- ^ Set v
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData, FromJSON, ToJSON, Serialize)

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
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData, FromJSON, ToJSON, Serialize)

data UnOp = Not -- ^ Logical negation
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData, FromJSON, ToJSON, Serialize)

-- | Literal representing Value
data Lit ac as c
  = LInt       Int64
  | LFloat     Double
  | LFixed     FixedN
  | LBool      Bool
  | LState     WorkflowState
  | LAccount   ac
  | LAsset     as
  | LContract  c
  | LText      SafeString
  | LSig       (SafeInteger,SafeInteger)
  | LDateTime  DateTime
  | LTimeDelta TimeDelta
  | LConstr    EnumConstr
  | LVoid
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData, FromJSON, ToJSON, Serialize)

-- | Values in which literals are evaluated to
data Value ac as c
  = VInt Int64                     -- ^ Integral types
  | VFloat Double                  -- ^ Floating types
  | VFixed FixedN                  -- ^ Fixed point types
  | VBool Bool                     -- ^ Boolean value
  | VAccount ac    -- ^ Account Address
  | VAsset as        -- ^ Asset Address
  | VContract c  -- ^ Contract Address
  | VText SafeString                -- ^ Msgs (ASCII)
  | VSig (SafeInteger,SafeInteger) -- ^ ESDSA Sig
  | VVoid                          -- ^ Void
  | VDateTime DateTime             -- ^ A datetime with a timezone
  | VTimeDelta TimeDelta           -- ^ A difference in time
  | VEnum EnumConstr               -- ^ Constructor of the given enum type
  | VState WorkflowState           -- ^ Named state label
  | VMap (Map
          (Value ac as c)
          (Value ac as c)
         )                         -- ^ Map of values to values
  | VSet (Set (Value ac as c))               -- ^ Set of values
  | VUndefined                     -- ^ Undefined
  deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hash.Hashable)

-- | Type variables used in inference
data TVar
  = TV  Text -- ^ General type variable
  | TAV Text -- ^ Type variable used for inferring return types of prim ops operating over assets
  | TCV Text -- ^ Type variable used for inferring return types of prim ops operating over collections
  | THV Type -- ^ Type variable used for inferring holdings type of polymorphic asset prim ops.
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, FromJSON, ToJSON)

data TAsset
  = TDiscrete         -- ^ Type of Discrete Assets
  | TBinary           -- ^ Type of Binary Assets
  | TFractional PrecN -- ^ Type of Fractional Assets
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, Serialize, FromJSON, ToJSON)

data TCollection
  = TMap Type Type  -- ^ Type of FCL Maps
  | TSet Type       -- ^ Type of FCL Sets
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, Serialize, FromJSON, ToJSON)

-- | Core types for FCL
data Type
  = TError          -- ^ (Internal) Error branch in typechecker
  | TVar TVar       -- ^ (Internal) Type variable used in inference
  | TAny            -- ^ (Internal) Polymorphic type
  | TInt            -- ^ Type of 64 bit integers
  | TFloat          -- ^ Type of double precision floats
  | TFixed PrecN    -- ^ Type of double precision floats
  | TBool           -- ^ Type of booleans
  | TAccount        -- ^ Type of account addresses
  | TAsset TAsset   -- ^ Type of asset addresses
  | TAssetAny       -- ^ Type of any asset
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
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, FromJSON, ToJSON, Serialize)

-- | Function argument
data Arg
  = Arg { argType :: Type, argName ::  LName }
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, FromJSON, ToJSON, Serialize)

data Precondition = PrecAfter | PrecBefore | PrecRoles
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, Serialize, FromJSON, ToJSON)

instance Pretty Precondition where
  ppr = \case
    PrecAfter  -> "after"
    PrecBefore -> "before"
    PrecRoles  -> "roles"

newtype Preconditions ac as c = Preconditions
  { unPreconditions :: [(Precondition, LExpr ac as c)] }
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, Serialize, FromJSON, ToJSON)

instance Semigroup (Preconditions ac as c) where
  Preconditions ps1 <> Preconditions ps2 = Preconditions $ ps1 <> ps2

instance Monoid (Preconditions ac as c) where
  mempty = Preconditions []

instance (Eq ac, Ord ac, Eq as, Eq c, Pretty ac, Pretty as, Pretty c) =>
  Pretty (Preconditions ac as c) where
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
data Method ac as c = Method
  { methodInputPlaces   :: WorkflowState
  , methodPreconditions :: Preconditions ac as c
  , methodName          :: LName
  , methodArgs          :: [Arg]
  , methodBody          :: LExpr ac as c
  } deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, FromJSON, ToJSON, Serialize)

-- | "Pure" Helper functions
data Helper ac as c = Helper
  { helperName :: LName
  , helperArgs :: [Arg]
  , helperBody :: LExpr ac as c
  } deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, FromJSON, ToJSON, Serialize)

-- | Enumeration
data EnumDef = EnumDef
  { enumName    :: LName
  , enumConstrs :: [LEnumConstr]
  } deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, ToJSON, FromJSON, Serialize)

-- | Definition
data Def ac as c
  = GlobalDef Type (Preconditions ac as c) Name (LExpr ac as c)
  | GlobalDefNull Type (Preconditions ac as c) LName
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, FromJSON, ToJSON, Serialize)

defnName :: Def ac as c -> Name
defnName = locVal . defnLName

defnLName :: Def ac as c -> LName
defnLName = \case
  GlobalDef _ _ nm _    -> Located NoLoc nm
  GlobalDefNull _ _ lnm -> lnm

defnPreconditions :: Def as ac c -> Maybe (Preconditions as ac c)
defnPreconditions = \case
    GlobalDef _ p _ _   -> Just p
    GlobalDefNull _ p _ -> Just p

-- | Script
data Script ac as c = Script
  { scriptEnums       :: [EnumDef]
  , scriptDefs        :: [Def ac as c]
  , scriptTransitions :: [Transition]
  , scriptMethods     :: [Method ac as c]
  , scriptHelpers     :: [Helper ac as c]
  } deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, ToJSON, FromJSON, Serialize)

emptyScript :: Script as ac c
emptyScript = Script
  { scriptEnums       = []
  , scriptDefs        = []
  , scriptTransitions = []
  , scriptMethods     = []
  , scriptHelpers     = []
  }

-- | Method args names and types
argtys :: Method as ac c -> [(Name,Type)]
argtys m = [(unLoc lnm, ty) | Arg ty lnm <- methodArgs m ]

argtys' :: Method as ac c -> [Type]
argtys' = map snd . argtys

-- | Method argument literals
argLits :: [Expr as ac c] -> [LLit as ac c]
argLits (ELit n : xs) = n : argLits xs
argLits (_ : xs)      = argLits xs
argLits []            = []

-- | Method names
methodNames :: Script as ac c -> [LName]
methodNames = fmap methodName . scriptMethods

lookupMethod :: Name -> Script as ac c -> Maybe (Method as ac c)
lookupMethod nm s =
  find ((==) nm . locVal . methodName) (scriptMethods s)

-- | Remove position information on an element
unLoc :: Located a -> a
unLoc (Located loc a) = a

-- | Put location information on an element
at :: a -> Loc -> Located a
at = flip Located

-- | Unroll a sequence of statements into a list.
unseq :: LExpr as ac c -> [LExpr as ac c]
unseq (Located l e) = case e of
  ESeq a b       -> unseq a ++ unseq b
  EIf cond tr fl -> unseq tr ++ unseq fl
  EBefore _ s    -> unseq s
  EAfter _ s     -> unseq s
  EBetween _ _ s -> unseq s
  ECase _ ms     -> concatMap (unseq . matchBody) ms
  _              -> [Located l e]

-- | Roll a list of expressions into a sequence.
eseq :: Loc -> [LExpr as ac c] -> LExpr as ac c
eseq loc es = case es of
  []     -> Located loc ENoOp
  [x]    -> x
  (x:xs) -> Located loc $
    ESeq x $ eseq (located x) xs

mapFixedType :: FixedN -> Type
mapFixedType fn =
  TFixed $ case fn of
    Fixed1 _ ->  Prec1
    Fixed2 _ ->  Prec2
    Fixed3 _ ->  Prec3
    Fixed4 _ ->  Prec4
    Fixed5 _ ->  Prec5
    Fixed6 _ ->  Prec6

-- @Nothing@ in case of an unknown constructor.
mapType :: EnumInfo -> Script.Value as ac c -> Maybe Type
mapType enumInfo (VEnum c)    = TEnum <$> Map.lookup c (constrToEnum enumInfo)
mapType _        VInt{}       = pure TInt
mapType _        VFloat{}     = pure TFloat
mapType _        (VFixed f)   = pure $ mapFixedType f
mapType _        VBool{}      = pure TBool
mapType _        VAccount{}   = pure TAccount
mapType _        VAsset{}     = pure TAssetAny
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
    []    -> pure (TColl (TSet TAny))
    (v:_) -> TColl <$> (TSet <$> mapType einfo v)

data EnumInfo = EnumInfo
  { constrToEnum  :: Map EnumConstr Name
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

instance (Ord ac, Ord as, Ord c, Serialize ac, Serialize as, Serialize c) =>
  Serialize (NonEmpty (LExpr ac as c)) where
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

instance (Eq ac, Eq as, Eq c, Pretty ac, Pretty as, Pretty c) => Pretty (Expr ac as c) where
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

instance (Eq ac, Eq as, Eq c, Pretty ac, Pretty as, Pretty c) =>
  Pretty (Match ac as c) where
  ppr (Match (Located _ (PatLit p)) expr)
    = notImplemented
    -- ppr (LConstr p) <+> token Token.rarrow <+> maybeBrace expr
    -- where
    --   -- Wrap braces around the expression in case it is a sequence of
    --   -- statements, otherwise don't.
    --   maybeBrace e
    --     = case locVal e of
    --         ESeq _ _ -> lbrace <+> semify (ppr e) <+> rbrace
    --         _        -> ppr e

instance (Pretty ac, Pretty as, Pretty c) => Pretty (Lit ac as c) where
  ppr = \case
    LInt int64     -> ppr int64
    LFloat dbl     -> ppr dbl
    LFixed mfix    -> ppr mfix
    LBool bool     -> ppr bool
    LText msg       -> dquotes $ ppr msg
    LAccount addr  -> "u" <> squotes (ppr addr)
    LAsset addr    -> "a" <> squotes (ppr addr)
    -- ^ TODO: Pretty Print addr as rawAddr
    LContract addr -> "c" <> squotes (ppr addr)
    LSig (r,s)     -> tupleOf [ppr r, ppr s]
    LVoid          -> token Token.void
    LState name    -> token Token.colon <> ppr name
    LDateTime dt   -> dquotes $ ppr $ (DT.formatDatetime (unDateTime dt) :: [Char])
    LTimeDelta d   -> ppr d
    LConstr ec     -> text "`" <> ppr ec

instance Pretty Type where
  ppr = \case
    TInt        -> token Token.int
    TFloat      -> token Token.float
    TFixed prec -> ppr prec
    TBool       -> token Token.bool
    TAny        -> token Token.any
    TAssetAny   -> token Token.asset
    TAccount    -> token Token.account
    TAsset TBinary   -> token Token.assetBin
    TAsset TDiscrete -> token Token.assetDis
    TAsset (TFractional Prec1) -> token Token.assetFrac1
    TAsset (TFractional Prec2) -> token Token.assetFrac2
    TAsset (TFractional Prec3) -> token Token.assetFrac3
    TAsset (TFractional Prec4) -> token Token.assetFrac4
    TAsset (TFractional Prec5) -> token Token.assetFrac5
    TAsset (TFractional Prec6) -> token Token.assetFrac6
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

instance (Pretty ac, Pretty as, Pretty c) => Pretty (Script.Value ac as c) where
  ppr = \case
    VInt n       -> ppr n
    VFloat n     -> ppr n
    VFixed n     -> ppr n
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

instance (Eq as, Eq c, Ord ac, Pretty ac, Pretty as, Pretty c) => Pretty (Method ac as c) where
  ppr (Method inputPs precs name args (Located _ body)) =
    token Token.at <> ppr inputPs <+> ppr precs
      <$$> ppr name <> tupleOf (map ppr args) <+>
      case body of
        ENoOp -> lbrace <+> rbrace
        e@(ESeq _ _) -> lbrace
          <$$> indent 2 (semify $ ppr e)
          <$$> rbrace
        other -> lbrace <$$> indent 2 (semify (ppr other)) <$$> rbrace

instance (Eq ac, Eq as, Eq c, Pretty ac, Pretty as, Pretty c) => Pretty (Helper ac as c) where
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

instance (Eq ac, Eq as, Eq c, Ord ac, Pretty ac, Pretty as, Pretty c) =>
  Pretty (Def ac as c) where
  ppr = \case
    GlobalDefNull typ precs (Located _ name)
      -> hsep [token Token.global, ppr typ, ppr precs, ppr name] <> token Token.semi
    GlobalDef typ precs name expr
      -> hsep [token Token.global, ppr typ, ppr precs, ppr name `assign` ppr expr]

instance (Eq as, Ord ac, Eq c, Pretty ac, Pretty as, Pretty c) =>
  Pretty (Script ac as c) where
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

ppScript
  :: (Ord as, Eq ac, Eq c, Pretty ac, Pretty as, Pretty c)
  => Script as ac c -> LText
ppScript = render . ppr

-------------------------------------------------------------------------------
-- Map Literals
-------------------------------------------------------------------------------

evalLit :: Lit as ac c -> Script.Value as ac c
evalLit lit = case lit of
  LInt n       -> VInt n
  LFloat n     -> VFloat n
  LFixed n     -> VFixed n
  LVoid        -> VVoid
  LBool n      -> VBool n
  LAccount n   -> VAccount n
  LAsset n     -> VAsset n
  LContract n  -> VContract n
  LText n      -> VText n
  LSig n       -> VSig n
  LState pl    -> VState pl
  LDateTime d  -> VDateTime d
  LTimeDelta d -> VTimeDelta d
  LConstr c    -> VEnum c

evalLLit :: LLit as ac c -> Script.Value as ac c
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
  deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hash.Hashable, ToJSON, FromJSON, FromJSONKey, ToJSONKey)

instance Pretty Place where
  ppr PlaceStart = ppr Token.initial
  ppr (Place id) = ppr id
  ppr PlaceEnd   = ppr Token.terminal

makePlace :: Name -> Place
makePlace nm@(Name n)
  | n == Token.initial = PlaceStart
  | n == Token.terminal = PlaceEnd
  | otherwise =  Place nm

startState, endState :: WorkflowState
startState = WorkflowState $ Set.singleton PlaceStart
endState = WorkflowState $ Set.singleton PlaceEnd

newtype WorkflowState = WorkflowState { places :: Set Place }
  deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hash.Hashable)

instance Pretty WorkflowState where
  ppr (Set.toList . places -> [p]) = ppr p
  ppr (Set.toList . places -> ps)  = listOf ps

instance ToJSON WorkflowState where
  toJSON = toJSON . prettyPrint

instance FromJSON WorkflowState where
  parseJSON = fmap (WorkflowState . Set.fromList . fmap (makePlace . Name) . T.splitOn (T.pack ",")) . parseJSON

data Transition
  = Arrow WorkflowState WorkflowState
  deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hash.Hashable, ToJSON, FromJSON)

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
