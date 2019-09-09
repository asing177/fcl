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
{-# LANGUAGE TypeFamilies #-}

module Language.FCL.AST (
  -- ** Syntax
  Script(..),
  Expr(..),
  Pattern(..),
  patLoc,
  CaseBranch(..),
  Method(..),
  Preconditions(..),
  Precondition(..),
  Helper(..),
  ADTDef(..),
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
  LNameUpper,
  LType,
  LPattern,
  -- ** Values
  Value(..),

  -- ** Name
  Name(..),
  NameUpper(..),
  defnName,
  defnLName,

  -- ** ADT constructor
  ADTConstr(..),

  -- ** State Labels
  Place(..),
  Transition(..),
  WorkflowState(..),
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
  ADTInfo(..),

  eseq,
  flattenExprs,
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

import qualified GHC.Exts as GHC (IsList(..))

import Test.QuickCheck hiding (listOf)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Instances.Text ()
import Generic.Random
import Control.Monad (fail)

import Numeric.Lossless.Number
import Language.FCL.Pretty
import Language.FCL.Prim (PrimOp)
import qualified Language.FCL.Pretty as Pretty
import qualified Language.FCL.Token as Token
import qualified Language.FCL.Hash as Hash
import qualified Data.Text as T
import qualified Datetime.Types as DT

import Data.Aeson as A hiding (Value)
import qualified Data.Binary as B
import Data.Char (isUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Serialize (Serialize(..), putInt8, getInt8)
import Data.Serialize.Text()

import Language.FCL.Address
import Language.FCL.Orphans ()

-------------------------------------------------------------------------------
-- Core Language
-------------------------------------------------------------------------------

data Loc
  = NoLoc
  | Loc { line :: Int, col :: Int }
  deriving (Eq, Show, Ord, Generic, Serialize, Hash.Hashable)

instance ToJSON Loc where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Loc where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

data Located a = Located { located :: Loc, locVal :: a }
  deriving (Generic, Show, FromJSONKey, ToJSONKey, Hash.Hashable)

instance ToJSON a => ToJSON (Located a) where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON a => FromJSON (Located a) where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- -- For debugging (reduces clutter)
-- instance Show a => Show (Located a) where
--   show = show . locVal

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
type LNameUpper = Located NameUpper
type LBinOp = Located BinOp
type LUnOp  = Located UnOp
type LPattern = Located Pattern

-- | ADT constructor as given in a type definition.
data ADTConstr = ADTConstr
  { adtConstrId :: LNameUpper, adtConstrParams :: [(LName, Type)] }
  deriving (Eq, Show, Ord, Generic, Hash.Hashable, Serialize)

instance ToJSON ADTConstr where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON ADTConstr where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Variable names
newtype Name = Name { unName :: Text }
  deriving (Eq, Show, Ord, Generic, B.Binary, Serialize, FromJSONKey, ToJSONKey, Hash.Hashable, NFData)

instance ToJSON Name where
  toJSON (Name nm) = toJSON nm

instance FromJSON Name where
  parseJSON = fmap Name . parseJSON

newtype NameUpper = MkNameUpper Text
  deriving (Eq, Show, Ord, Generic, B.Binary, Serialize, Hash.Hashable)

instance ToJSON NameUpper where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON NameUpper where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Datetime literals
newtype DateTime = DateTime { unDateTime :: DT.Datetime }
  deriving (Eq, Ord, Show, Generic, Serialize)

newtype TimeDelta = TimeDelta { unTimeDelta :: DT.Delta }
   deriving (Eq, Ord, Show, Generic, Serialize)

instance ToJSON TimeDelta where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON TimeDelta where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Hash.Hashable TimeDelta where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

instance Hash.Hashable DateTime where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

-- | FCL pattern language for @case@ matches.
data Pattern
  = PatConstr LNameUpper [Pattern] -- ^ Constructor pattern
  | PatLit LLit                    -- ^ Literal pattern
  | PatVar LName                   -- ^ Variable pattern
  | PatWildCard                    -- ^ Wildcard ("don't care") pattern
  -- TODO: PatOr Pattern Pattern
  deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON Pattern where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Pattern where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Retrieve the location associated with a pattern ('PatWildCard' returns 'NoLoc')
patLoc :: Pattern -> Loc
patLoc = \case
  PatConstr nm _ -> located nm
  PatLit lit -> located lit
  PatVar nm -> located nm
  PatWildCard -> NoLoc

-- | A case branch, consisting of a pattern and a branch body
data CaseBranch
  = CaseBranch{ matchPat :: LPattern, matchBody :: LExpr }
  deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON CaseBranch where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON CaseBranch where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | FCL expressions
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
  | ECase    LExpr [CaseBranch]    -- ^ Case statement
  | EAssign  (NonEmpty Name)  LExpr -- ^ Variable update
  | ECall    (Either PrimOp LName) [LExpr] -- ^ Function call
  | ENoOp                          -- ^ Empty method body
  | EMap     (Map LExpr LExpr)     -- ^ Map k v
  | ESet     (Set LExpr)           -- ^ Set v
  | EConstr NameUpper [LExpr]      -- ^ Record constructor
  deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON Expr where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Expr where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Binary operators
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
  | RecordAccess -- ^ Record access, e.g. @expr.field@
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, Bounded, Enum)

instance ToJSON BinOp where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON BinOp where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Unary operators
data UnOp = Not -- ^ Logical negation
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, Bounded, Enum)

instance ToJSON UnOp where
  toJSON _ = "Not"

instance FromJSON UnOp where
  parseJSON _ = pure Not

-- | Literal representing Value
data Lit
  = LNum       Decimal
  | LBool      Bool
  | LState     WorkflowState
  | LAccount   (Address AAccount)
  | LAsset     (Address AAsset)
  | LContract  (Address AContract)
  | LText      Text
  | LSig       (Integer, Integer)
  | LDateTime  DateTime
  | LTimeDelta TimeDelta
  -- | LConstr    Name
  deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON Lit where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Lit where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Values in which literals are evaluated to
data Value
  = VNum Number                    -- ^ Number
  | VBool Bool                     -- ^ Boolean value
  | VAccount (Address AAccount)    -- ^ Account Address
  | VAsset (Address AAsset)        -- ^ Asset Address
  | VContract (Address AContract)  -- ^ Contract Address
  | VText Text                     -- ^ Msgs (ASCII)
  | VSig (Integer, Integer)        -- ^ ESDSA Sig
  | VVoid                          -- ^ Void
  | VDateTime DateTime             -- ^ A datetime with a timezone
  | VTimeDelta TimeDelta           -- ^ A difference in time
  | VState WorkflowState           -- ^ Named state label
  | VMap (Map Value Value)         -- ^ Map of values to values
  | VSet (Set Value)               -- ^ Set of values
  | VUndefined                     -- ^ Undefined
  | VConstr NameUpper [Value]      -- ^ Constructor
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable)

instance ToJSONKey Value where

instance ToJSON Value where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Value where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSONKey Value where

-- | Type variables used in inference
data TVar
  = TV  Text -- ^ General type variable
  | TAV Text -- ^ Type variable used for inferring return types of prim ops operating over assets
  | TCV Text -- ^ Type variable used for inferring return types of prim ops operating over collections
  | THV Type -- ^ Type variable used for inferring holdings type of polymorphic asset prim ops.
  deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON TVar where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON TVar where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Collection types
data TCollection
  = TMap Type Type  -- ^ Type of FCL Maps
  | TSet Type       -- ^ Type of FCL Sets
  deriving (Eq, Ord, Show, Generic, Serialize, FromJSON, ToJSON, Hash.Hashable)

instance Arbitrary TCollection where
  arbitrary = genericArbitraryU

-- | The required numeric precision @p@ to ensure non-lossy arithmetic. This is
-- tracked in the type of numbers, @TNum p@.
data NumPrecision
  = NPArbitrary                      -- ^ arbitrary precision
  | NPDecimalPlaces Integer          -- ^ fixed number of decimal places
  | NPAdd NumPrecision NumPrecision  -- ^ internal (for multiplication)
  | NPMax NumPrecision NumPrecision  -- ^ internal (for addition)
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable)

instance ToJSON NumPrecision where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON NumPrecision where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Smart constructor for 'NumPrecision' for integers
nPInt :: NumPrecision
nPInt = NPDecimalPlaces 0

-- | Map 'NumPrecision' values into its base cases ('NPArbitrary' and
-- 'NPDecimalPlaces')
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
  | TNum NumPrecision -- ^ Type of rational numbers
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
  | TADT Name       -- ^ User-declared algebraic data type
  | TFun [Type] Type -- ^ Type signature of helper functions--argument types and return type
  | TColl TCollection -- ^ Type of collection values
  | TTransition     -- ^ Transition type
  deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON Type where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Type where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Function argument
data Arg
  = Arg { argType :: Type, argName ::  LName }
  deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON Arg where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Arg where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

data Precondition = PrecAfter | PrecBefore | PrecRoles
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable)

instance ToJSON Precondition where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Precondition where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance Pretty Precondition where
  ppr = \case
    PrecAfter  -> "after"
    PrecBefore -> "before"
    PrecRoles  -> "roles"

newtype Preconditions = Preconditions
  { unPreconditions :: [(Precondition, LExpr)] }
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable)

instance GHC.IsList Preconditions where
  type Item Preconditions = (Precondition, LExpr)

  fromList xs = Preconditions xs

  toList (Preconditions xs) = xs

instance ToJSON Preconditions where
  toJSON (Preconditions p) = toJSON p

instance FromJSON Preconditions where
  parseJSON = fmap Preconditions . parseJSON

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
  } deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON Method where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Method where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | "Pure" Helper functions
data Helper = Helper
  { helperName :: LName
  , helperArgs :: [Arg]
  , helperBody :: LExpr
  } deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON Helper where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Helper where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | ADT
data ADTDef = ADTDef
  { adtName :: LName
  , adtConstrs :: NonEmpty ADTConstr
  } deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON ADTDef where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON ADTDef where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

-- | Definition
data Def
  = GlobalDef
    { gDefType :: Type
    , gDefPrec :: Preconditions
    , gDefName :: Name
    , gDefExpr :: LExpr
    }
  | GlobalDefNull
    { gDefNullType :: Type
    , gDefNullPrec :: Preconditions
    , gDefNullName :: LName
    }
  deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON Def where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Def where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

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
  { scriptADTs       :: [ADTDef]
  , scriptDefs        :: [Def]
  , scriptTransitions :: [Transition]
  , scriptMethods     :: [Method]
  , scriptHelpers     :: [Helper]
  } deriving (Eq, Ord, Show, Generic, Hash.Hashable)

instance ToJSON Script where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Script where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

emptyScript :: Script
emptyScript = Script
  { scriptADTs       = []
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
flattenExprs :: LExpr -> [LExpr]
flattenExprs (Located l e) = case e of
  ESeq a b       -> flattenExprs a ++ flattenExprs b
  EIf cond tr fl -> flattenExprs tr ++ flattenExprs fl
  EBefore _ s    -> flattenExprs s
  EAfter _ s     -> flattenExprs s
  EBetween _ _ s -> flattenExprs s
  ECase _ ms     -> concatMap (flattenExprs . matchBody) ms
  _              -> [Located l e]

-- | Roll a list of expressions into a sequence.
eseq :: Loc -> [LExpr] -> LExpr
eseq loc es = case es of
  []     -> Located loc ENoOp
  [x]    -> x
  (x:xs) -> Located loc $
    ESeq x $ eseq (located x) xs

-- @Nothing@ in case of an unknown constructor.
mapType :: ADTInfo -> Value -> Maybe Type
mapType _        (VNum (NumRational _)) = pure (TNum NPArbitrary)
mapType _        (VNum (NumDecimal f))  = (pure . TNum . NPDecimalPlaces . decimalPlaces) f
mapType adtInfo (VConstr c _) = TADT . locVal . fst <$> Map.lookup c (constructorToType adtInfo)
mapType _        VBool{}      = pure TBool
mapType _        VAccount{}   = pure TAccount
mapType _        VAsset{}     = pure (TAsset TAny)
mapType _        VContract{}  = pure TContract
mapType _        VVoid        = pure TVoid
mapType _        VText{}      = pure TText
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

-- | Associations between type- and value-constructors
data ADTInfo = ADTInfo
  { constructorToType :: Map NameUpper (LName, [(LName, Type)])
  , adtToConstrsAndFields :: Map Name (NonEmpty ADTConstr, [(Name, Type)])
  }

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance IsString Name where
  fromString "" = panic "empty name"
  fromString s@(c:_)
    | isUpper c = panic "expected lowercase name"
    | otherwise = Name $ toS s

instance IsString NameUpper where
  fromString "" = panic "empty name"
  fromString s@(c:_)
    | isUpper c = MkNameUpper $ toS s
    | otherwise = panic "expected uppercase name"

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

instance (Serialize a) => Serialize (Located a) where
  put (Located loc x) = put (loc,x)
  get = uncurry Located <$> get

instance Serialize Lit where
instance Serialize ADTDef where
instance Serialize Def where
instance Serialize Arg where
instance Serialize Type where
instance Serialize Pattern where
instance Serialize CaseBranch where
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

instance Pretty NameUpper where
  ppr (MkNameUpper nm) = ppr nm

instance Pretty ADTConstr where
  ppr (ADTConstr id []) = ppr id
  ppr (ADTConstr id namedTyParams)
    = ppr id
      <> tupleOf (map (\(nm, ty) -> ppr ty <+> ppr nm) namedTyParams)

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
    EAssign nms e    -> (mconcat . intersperse "." . map ppr . toList) nms <+> token Token.assign <+> ppr e
    EUnOp nm e       -> parens $ ppr nm <> ppr e
    EBinOp (Located _ RecordAccess) e1 e2
      -> ppr e1 <> token Token.dot <> ppr e2
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
    EConstr nm [] -> ppr nm
    EConstr nm es -> ppr nm <> tupleOf es

instance Pretty Pattern where
  ppr = \case
    PatConstr id pats -> ppr id <> tupleOf pats
    PatLit lit -> ppr lit
    PatVar v -> ppr v
    PatWildCard -> "_"

instance Pretty CaseBranch where
  ppr (CaseBranch pat expr)
    = ppr pat <+> token Token.rarrow <+> maybeBrace expr
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
    LBool bool     -> if bool then token Token.true else token Token.false
    LText msg       -> dquotes $ ppr msg
    LAccount addr  -> "u" <> squotes (ppr addr)
    LAsset addr    -> "a" <> squotes (ppr addr)
    LContract addr -> "c" <> squotes (ppr addr)
    LSig (r,s)     -> tupleOf [ppr r, ppr s]
    LState name    -> token Token.at <> ppr name
    LDateTime dt   -> dquotes $ ppr $ (DT.formatDatetime (unDateTime dt) :: [Char])
    LTimeDelta d   -> ppr d

instance Pretty Type where
  ppr = \case
    TNum p      -> case normaliseNumPrecision p of
        NPArbitrary         -> token Token.num
        (NPDecimalPlaces 0) -> token Token.int
        (NPDecimalPlaces p) -> token Token.decimal <> angles (ppr p)
        _                   -> panic $ "Expected normalised num precision"
    TBool       -> token Token.bool
    TAny        -> "<any-type>" -- not in syntax
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
    TState      -> "<workflow-state>" -- not in syntax
    TADT e     -> ppr e
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
    VState n     -> ppr n
    VMap vmap    -> ppr vmap
    VSet vset    -> tupleOf (Set.toList vset)
    VConstr nm [] -> ppr nm
    VConstr nm vs -> ppr nm <> tupleOf vs
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
    RecordAccess -> token Token.dot

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

instance Pretty ADTDef where
  ppr (ADTDef lname lconstrsAndTypes)
    = token Token.type_ <+> ppr (locVal lname) <+> lbrace
      <$$> indent 2 (semify . hsep . punctuate "; " . map ppr $ toList lconstrsAndTypes)
      <$$> rbrace

instance Pretty Def where
  ppr = \case
    GlobalDefNull typ precs (Located _ name)
      -> hsep [token Token.global, ppr typ, ppr precs, ppr name] <> token Token.semi
    GlobalDef typ precs name expr
      -> hsep [token Token.global, ppr typ, ppr precs, ppr name `assign` ppr expr]

instance Pretty Script where
  ppr (Script adts defns transitions methods functions) = vsep
    [ vsep (map ppr adts)
    , vsep (map ppr defns)
    , if null transitions
      then mempty
      else vsep [Pretty.softbreak, ppr transitions]
    , Pretty.softbreak
    , vsep (spaced (map ppr methods))
    , Pretty.softbreak
    , vsep (spaced (map ppr functions))
    ]

ppScript :: Script -> LText
ppScript = render . ppr

-------------------------------------------------------------------------------
-- To/FromJSON
-------------------------------------------------------------------------------

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
  deriving (Eq, Ord, Show, Generic, Serialize, FromJSONKey, ToJSONKey, Hash.Hashable, NFData)

instance ToJSON Place where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Place where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })


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
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable, NFData)

instance Semigroup WorkflowState where
  (<>) (WorkflowState xs) (WorkflowState ys) = WorkflowState (xs <> ys)

instance Monoid WorkflowState where
  mempty = WorkflowState mempty

instance Pretty WorkflowState where
  ppr (Set.toList . places -> [p]) = ppr p
  ppr (Set.toList . places -> ps) = listOf ps

instance ToJSON WorkflowState where
  toJSON = toJSON . prettyPrint

instance FromJSON WorkflowState where
  parseJSON wf = parseJSON wf >>= \t
    -> pure $ case (T.take 1 t, T.takeEnd 1 t) of
         ("{", "}") -> WorkflowState . Set.fromList . fmap (makePlace . Name . T.strip) . T.splitOn (T.pack ",") . T.dropEnd 1 . T.drop 1 $ t
         _ -> WorkflowState . Set.fromList . fmap (makePlace . Name . T.strip) . T.splitOn (T.pack ",") $ t

data Transition
  = Arrow WorkflowState WorkflowState
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable, NFData)

instance ToJSON Transition where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON Transition where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

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


makeWorkflowState :: [Name] -> WorkflowState
makeWorkflowState = WorkflowState . Set.fromList . map makePlace

-- | Doesn't check if workflow state is valid
unsafeWorkflowState :: Set Place -> WorkflowState
unsafeWorkflowState = WorkflowState

---------------
-- Arbitrary --
---------------

arbValue :: Int -> Gen Value
arbValue n
  | n <= 0
    = oneof
      [ VNum <$> arbitrary
      , VBool <$> arbitrary
      , VAccount <$> arbitrary
      , VAsset <$> arbitrary
      , VText <$> arbitrary
      , VSig <$> arbitrary
      , VDateTime <$> arbitrary
      , VTimeDelta <$> arbitrary
      , VState <$> arbitrary
      , pure VUndefined
      ]
  | otherwise
    = oneof
      [ VMap . Map.fromList <$> Q.listOf (liftArbitrary2 (arbValue (n - 1)) (arbValue (n - 1)))
      , VSet . Set.fromList <$> Q.listOf (arbValue (n - 1))
      , VConstr <$> arbitrary <*> arbSmallList
      ]

instance Arbitrary Value where
  arbitrary = oneof [arbValue 0, arbValue 1]

instance Arbitrary DateTime where
  arbitrary = DateTime <$> arbitrary

instance Arbitrary TimeDelta where
  arbitrary = TimeDelta <$> arbitrary

instance Arbitrary Name where
  arbitrary = fromString
    <$> ((:) <$> elements ['a'..'z'] <*> Q.listOf alphaNum)
      `suchThat` (not . (`elem` Token.keywords) . toS)

instance Arbitrary NameUpper where
  arbitrary = fromString <$> ((:) <$> elements ['A'..'Z'] <*> Q.listOf alphaNum)

alphaNum :: Gen Char
alphaNum = elements $ ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9']

instance Arbitrary Place where
  arbitrary = Place <$> arbitrary

instance Arbitrary WorkflowState where
  arbitrary = oneof
    [ unsafeWorkflowState <$> do
        hd <- arbitrary
        tl <- arbSmallList
        pure $ Set.fromList (hd:tl)
    , pure startState
    , pure endState
    ]


instance Arbitrary Loc where
  arbitrary = Loc <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary

-- This is basically liftArbitrary from Arbitrary1
addLoc :: Gen a -> Gen (Located a)
addLoc g = Located <$> arbitrary <*> g

instance Arbitrary UnOp where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Lit where
  -- Missing literals:
  --  + LDateTime: missing instance Arbitrary DateTime (!)
  --  + LTimeDelta: missing instance Arbitrary TimeDelta (!)
  --  + LSig: not part of concrete syntax
  arbitrary = oneof
    [ LNum <$> arbitrary `suchThat` ((>= 0) . decimalPlaces)
      -- specifically, for 'LNum', ensure that we choose the 'NumDecimal' case
      -- and not 'NumRational', since there are no rational literals.
    , LBool     <$> arbitrary
    , LState    <$> arbitrary
    , LAccount  <$> arbitrary
    , LAsset    <$> arbitrary
    , LContract <$> arbitrary
    , LDateTime <$> arbitrary
    -- , LTimeDelta <$> arbitrary
    ]

instance Arbitrary Type where
  arbitrary = oneof
    [ TNum <$> arbitrary
    , pure TBool
    , pure TAccount
    , TAsset <$> arbitrary
    , pure TContract
    , TADT <$> arbitrary
    ]

instance Arbitrary NumPrecision where
  arbitrary = oneof
    [ pure NPArbitrary
    , NPDecimalPlaces <$> arbitrary
    ]

instance Arbitrary Def where
  arbitrary = oneof
    [ GlobalDef <$> arbitrary <*> arbitrary <*> arbitrary <*> addLoc (sized arbNonSeqExpr)
    ]

instance Arbitrary Arg where
  arbitrary = Arg <$> arbitrary <*> arbitrary

instance Arbitrary Preconditions where
  arbitrary = do
    exprs <- infiniteListOf (Located NoLoc <$> arbNonSeqExpr 0)
    precs <- infiniteListOf arbitrary
    n <- choose (0, 3)
    pure . Preconditions . take n $ zip precs exprs

instance Arbitrary Precondition where
  arbitrary = elements [ PrecAfter, PrecBefore, PrecRoles ]

instance Arbitrary Method where
  arbitrary = Method <$> arbitrary <*> arbitrary <*> arbitrary <*> arbSmallList <*> sized arbLExpr

instance Arbitrary Helper where
  arbitrary = Helper <$> arbitrary <*> arbSmallList <*> arbitrary

instance Arbitrary Transition where
  arbitrary = Arrow <$> arbitrary <*> arbitrary

instance Arbitrary ADTDef where
  arbitrary = ADTDef <$> arbitrary <*> arbitrary `suchThat` (\x -> length x > 0 && length x < 10)

instance Arbitrary ADTConstr where
  arbitrary = ADTConstr <$> arbitrary <*> arbTake 5 (Q.listOf arbitraryParam)
    where
      arbitraryParam = (,) <$> arbitrary <*> arbitrary

instance Arbitrary Script where
  arbitrary
    = Script
      <$> arbSmallList
      <*> arbSmallList
      <*> arbSmallList
      <*> arbSmallList
      <*> arbSmallList

  shrink = genericShrink

instance Arbitrary Expr where
  arbitrary = do
    n <- choose ((-5), 5)
    arbNonSeqExpr n

instance Arbitrary Pattern where
  arbitrary = genericArbitraryU

instance Arbitrary BinOp where
  arbitrary = genericArbitraryU

arbNumLogicExpr :: Int -> Gen Expr
arbNumLogicExpr n
  | n <= 0
    = oneof $ [EVar <$> arbitrary] ++
      map (fmap ELit . addLoc)
            [ LNum <$> arbitrary
            , LBool <$> arbitrary
            ]
  | otherwise = let n' = n `div` 2 in oneof
      [ EBinOp <$> addLoc (elements (enumFromTo Add Greater)) -- exclude RecordAccess
               <*> addLoc (arbNumLogicExpr n')
               <*> addLoc (arbNumLogicExpr n')
      , EUnOp <$> arbitrary <*> addLoc (arbNumLogicExpr n')
      ]

arbMatches :: Int -> Gen [CaseBranch]
arbMatches n = arbTake 5 (listOf1 (CaseBranch <$> arbPat <*> arbLExpr n))

arbPat :: Gen LPattern
arbPat = Located <$> arbitrary <*> (PatLit <$> arbitrary)

arbNonSeqExpr :: Int -> Gen Expr
arbNonSeqExpr n
  | n <= 0 = oneof
             [ EVar <$> arbitrary
             , ELit <$> arbitrary
             ]
  | otherwise = let n' = n `div` 2 in oneof
      [ EAssign <$> arbitrary         <*> addLoc (arbNonSeqExpr n')
      , ECall   <$> arbitrary         <*> arbTake 5 (Q.listOf (addLoc (arbNonSeqExpr n')))
      , EIf     <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n' <*> arbLExpr n'
      , EBefore <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'
      , EAfter  <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'
      , EBetween <$> addLoc (arbNonSeqExpr n')
                 <*> addLoc (arbNonSeqExpr n')
                 <*> arbLExpr n'
      , ECase <$> addLoc (arbNonSeqExpr n') <*> arbMatches n'
      , EBinOp <$> addLoc (pure RecordAccess)
               <*> addLoc (EVar <$> arbitrary)
               <*> addLoc (EVar <$> arbitrary)
      , arbNumLogicExpr n
      ]

arbSeqExpr :: Int -> Gen Expr
arbSeqExpr n
  | n <= 0 = arbNonSeqExpr 0
  | otherwise = let n' = n `div` 2 in
      ESeq <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'

arbLExpr :: Int -> Gen LExpr
arbLExpr n = oneof . map addLoc $
  [ arbNonSeqExpr n, arbSeqExpr n ]

arbSmallList :: Arbitrary a => Gen [a]
arbSmallList = arbitrary `suchThat` (\x -> length x < 5)

arbTake :: Int -> Gen [a] -> Gen [a]
arbTake n arb = arb `suchThat` (\x -> length x < n)
