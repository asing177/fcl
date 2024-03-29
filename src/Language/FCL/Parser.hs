{-|

Parser for the FCL scripting language.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.FCL.Parser (
  -- ** Parser
  parseExpr,
  parseScript,
  parseText,
  parseFile,

  parseADTDef,
  parseDefn,
  parseLit,
  parseDecimal,
  parseType,
  parseTimeDelta,
  parseDateTime,
  parseWorkflowState,
  parseBlock,
  parseAdtDef,
  parseCall,
  parseMethod,

  expr,
  callExpr,
  datetimeParser,

  -- ** Parser Errors
  ParseError,
  ParseErrInfo(..),
  mkParseErrInfo,
  errorMessages,
  messageString,

  decimal,
  decimalLit,
  textLit,
  contents,
  arg,
  lit,
  commaSep,
  parens,
  name,
  block,

  testParse,
  ) where

import Protolude hiding
  ((<|>), (<>), bool, many, try, option, optional, sourceLine, sourceColumn, Type)
import Prelude (read)
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr as Expr
import Text.ParserCombinators.Parsec.Error
import qualified Text.Parsec.Token as Tok

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Data.Foldable (foldr1)
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Numeric.Lossless.Number
import Language.FCL.AST hiding (mapType)
import Language.FCL.Address
import Language.FCL.Lexer as Lexer
import Language.FCL.Pretty hiding (parens, comma)
import Language.FCL.Prim (lookupPrim, PrimOp)
import qualified Language.FCL.Token as Token
import qualified Datetime.Types as DT

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- | Parse an expression.
parseExpr :: T.Text -> Either ParseErrInfo LExpr
parseExpr input = first (mkParseErrInfo input)
  $ parse (contents expr) "<stdin>" input

-- | Parse file contents into a Language.FCL.
parseScript :: T.Text -> Either ParseErrInfo Script
parseScript input = first (mkParseErrInfo input)
  $ parse (contents script <* eof) "<stdin>" input

-- | Parse text not expecting eof
parseText :: Text -> Either ParseErrInfo Script
parseText input = first (mkParseErrInfo input)
  $ parse (contents script) mempty input

-- | Parse a file into a Language.FCL.
parseFile :: FilePath -> IO Script
parseFile path = do
  eScript <- parseScript <$> readFile path
  either panicppr pure eScript

parseADTDef :: Text -> Either ParseErrInfo ADTDef
parseADTDef input = first (mkParseErrInfo input)
  $ parse (contents adtDef) "ADT" input

parseDefn :: Text -> Either ParseErrInfo Def
parseDefn input = first (mkParseErrInfo input)
  $ parse (contents def) "definition" input

parseMethod :: Text -> Either ParseErrInfo Method
parseMethod input = first (mkParseErrInfo input)
  $ parse (contents method) "method" input

parseLit :: Text -> Either ParseErrInfo Lit
parseLit input = first (mkParseErrInfo input)
  $ parse (contents lit) "literal" input

parseDecimal :: Text -> Either ParseErrInfo Decimal
parseDecimal input = first (mkParseErrInfo input)
  $ parse (contents decimal) "decimal" input

parseType :: Text -> Either ParseErrInfo Type
parseType input = first (mkParseErrInfo input)
  $ parse (contents type_) "type" input

parseTimeDelta :: Text -> Either ParseErrInfo TimeDelta
parseTimeDelta input = first (mkParseErrInfo input)
  $ parse timedeltaParser "timedelta" input

parseDateTime :: Text -> Either ParseErrInfo DateTime
parseDateTime input = first (mkParseErrInfo input)
  $ parse datetimeParser "datetime" input

parseWorkflowState :: Text -> Either ParseErrInfo WorkflowState
parseWorkflowState input = first (mkParseErrInfo input)
  $ parse workflowPlaces "workflowPlaces" input

parseBlock :: T.Text -> Either ParseErrInfo LExpr
parseBlock input = first (mkParseErrInfo input)
  $ parse (contents block) "block" input

parseAdtDef :: T.Text -> Either ParseErrInfo ADTDef
parseAdtDef input = first (mkParseErrInfo input)
  $ parse (contents adtDef) "adtDef" input

parseCall :: Text -> Either ParseErrInfo ((Either PrimOp LName), [LExpr])
parseCall input = first (mkParseErrInfo input) $ parse call "call" input

contents :: Parser a -> Parser a
contents p = whiteSpace *> p

testParse :: T.Text -> IO ()
testParse = parseTest (contents script)


-------------------------------------------------------------------------------
-- Lit
-------------------------------------------------------------------------------


lit :: Parser Lit
lit =  try timedeltaLit
   <|> decimalLit
   <|> boolLit
   <|> addressLit
   <|> stateLit
   <|> datetimeLit
   <|> textLit
   <?> "literal"

locLit :: Parser LLit
locLit = mkLocated lit

decimalLit :: Parser Lit
decimalLit = LNum <$> decimal

-- This should be isomorphic to the 'Read' instance of 'Decimal'.
decimal :: Parser Decimal
decimal = do
    sig <- sign
    int <- many1 $ satisfy isDigit
    dec <- maybe "" identity <$> optionMaybe (char '.' *> many1 (satisfy isDigit))
    exp <- maybe 0 read <$> optionMaybe (do
                          _ <- char 'e'
                          sig <- sign
                          n <- many1 (satisfy isDigit)
                          pure (sig <> n))
    _ <- whiteSpace
    pure
      . Decimal (genericLength dec - exp)
      . read
      $ sig <> int <> dec
  where
    sign = maybe "" pure <$> optionMaybe (char '-')

-- for backwards compatibility, support capitalised version
boolLit :: Parser Lit
boolLit =
     LBool False <$ try (reserved Token.false)
 <|> LBool True  <$ try (reserved Token.true)
 <?> "boolean literal"

rawAddress :: forall (a :: AddrType). Parser (Address a)
rawAddress =
  Address . BS8.pack <$> between (symbol "\'") (symbol "\'") (many1 alphaNum)

addressLit :: Parser Lit
addressLit = try $ do
  type_ <- char 'c' <|> char 'a' <|> char 'u'
  if | type_ == 'c' -> LContract <$> rawAddress
     | type_ == 'a' -> LAsset <$> rawAddress
     | type_ == 'u' -> LAccount <$> rawAddress
     | otherwise    -> parserFail "Cannot parse address literal"

datetimeParser :: Parser DateTime
datetimeParser = try $ do
  isoStr <- rawTextLit
  case DT.parseDatetime isoStr of
    Nothing -> parserFail "Invalid ISO8601 datetime string"
    Just datetime -> case DT.validateDatetime datetime of
      Left err -> parserFail "Invalid datetime specified"
      Right _ -> pure $ DateTime datetime

datetimeLit :: Parser Lit
datetimeLit = LDateTime <$> datetimeParser

timedeltaParser :: Parser TimeDelta
timedeltaParser = do
    years  <- DT.years  <$> parseYear
    months <- DT.months <$> parseMonth
    days   <- DT.days   <$> parseDay
    hours  <- DT.hours  <$> parseHour
    mins   <- DT.mins   <$> parseMin
    secs   <- DT.secs   <$> parseSec
    let delta = mconcat [ years, months, days, hours, mins, secs ]
    if delta == DT.years 0
      then parserFail "TimeDelta must not be 0"
      else pure $ TimeDelta delta
  where
    parseNat suffix = fmap fromIntegral $
      option 0 (try $ Tok.natural lexer <* string suffix)

    parseYear  = parseNat "y"
    parseMonth = parseNat "mo"
    parseDay   = parseNat "d"

    parseHour  = parseNat "h"
    parseMin   = parseNat "m"
    parseSec   = parseNat "s"

timedeltaLit :: Parser Lit
timedeltaLit = LTimeDelta <$> timedeltaParser

textLit :: Parser Lit
textLit = LText . toS . BS8.pack <$> rawTextLit
 <?> "text"

stateLit :: Parser Lit
stateLit = do
  char '@' <|> char ':'
  LState <$> workflowPlaces
 <?> "state literal"

rawTextLit :: Parser [Char]
rawTextLit = do
    txt <- between
      (symbol "\"")
      (symbol "\"")
      (many ascii)
    return txt
 <?> "text literal"
  where
    ascii = alphaNum
         <|> (oneOf $ "!@#$%^&*()-=_+[]{};:',<.>/?\\| ")

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type_ :: Parser Type
type_ =  intType
     <|> numType
     <|> decimalType
     <|> boolType
     <|> accountType
     <|> assetType
     <|> contractType
     <|> sigType
     <|> textType
     <|> dateType
     <|> timedeltaType
     <|> collectionType
     <|> adtType
     <?> "type"

intType :: Parser Type
intType = TNum nPInt <$ try (reserved Token.int)

numType :: Parser Type
numType = TNum NPArbitrary <$ try (reserved Token.num)

decimalType :: Parser Type
decimalType = do
  try $ reserved Token.decimal
  symbol "<"
  n <- Tok.integer lexer
  symbol ">"
  pure . TNum . NPDecimalPlaces $ n

boolType :: Parser Type
boolType = TBool <$ try (reserved Token.bool)

accountType :: Parser Type
accountType = TAccount <$ try (reserved Token.account)

assetType :: Parser Type
assetType = do
    try (reserved Token.asset)
    symbol "<"
    t <- type_
    symbol ">"
    pure $ TAsset t

contractType :: Parser Type
contractType = TContract <$ try (reserved Token.contract)

sigType :: Parser Type
sigType = TSig <$ try (reserved Token.sig)

textType :: Parser Type
textType = TText <$ try (reserved Token.text)

dateType :: Parser Type
dateType = TDateTime <$ try (reserved Token.datetime)

timedeltaType :: Parser Type
timedeltaType = TTimeDelta <$ try (reserved Token.timedelta)

adtType :: Parser Type
adtType = TADT <$> Lexer.name

collectionType :: Parser Type
collectionType =
  fmap TColl $  mapType
            <|> setType

mapType :: Parser TCollection
mapType = do
  try (reserved Token.map)
  a <- symbol "<" *> type_
  lexeme comma
  b <- type_ <* symbol ">"
  pure (TMap a b)

setType :: Parser TCollection
setType = do
  try (reserved Token.set)
  TSet <$> (symbol "<" *> type_ <* symbol ">")

-------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------

def :: Parser Def
def = do
    _ <- try (reserved Token.global <|> reserved Token.local)
    typ <- type_
    precs <- preconditions <|> pure (mempty @Preconditions)
    Located loc id <- locName
    let
      initDef = do
        reservedOp Token.assign
        lexpr <- expr
        pure $ GlobalDef typ precs id lexpr
      nullDef = pure $ GlobalDefNull typ precs (Located loc id)
    initDef <|> nullDef

-------------------------------------------------------------------------------
-- Methods & Helper Functions
-------------------------------------------------------------------------------

arg :: Parser Arg
arg = Arg <$> type_ <*> locName
   <?> "argument"

method :: Parser Method
method = do
  LState (inputPlaces) <- stateLit
  precs <- preconditions <|> pure (mempty @Preconditions)
  loc <- location
  nm <- name
  args <- parens $ commaSep arg
  body <- block
  return $ Method inputPlaces precs (Located loc nm) args body
 <?> "method"

preconditions :: Parser Preconditions
preconditions = Preconditions <$> (brackets . commaSep $ do
  p <- precondition
  char ':'
  whiteSpace
  e <- expr
  return (p,e))

precondition :: Parser Precondition
precondition
  =   (reserved "after"  *> pure PrecAfter)
  <|> (reserved "before" *> pure PrecBefore)
  <|> (reserved "role"   *> pure PrecRoles)
  <|> (reserved "roles"  *> pure PrecRoles)

helper :: Parser Helper
helper =  Helper
      <$> Lexer.locName
      <*> parens (commaSep arg)
      <*> block

-------------------------------------------------------------------------------
-- Exprs
-------------------------------------------------------------------------------

binOp :: BinOp -> Expr.Assoc -> Operator Text () Identity LExpr
binOp nm = Expr.Infix (locBinOp nm)

locBinOp :: BinOp -> Parser (LExpr -> LExpr -> LExpr)
locBinOp nm = do
  loc <- location
  opName <- mkLocated $ op nm
  return $ \le1 le2 ->
    Located loc $ EBinOp opName le1 le2

binOpTry :: BinOp -> BinOp -> Expr.Assoc -> Operator Text () Identity LExpr
binOpTry nm nm2 = Expr.Infix (locBinOpTry nm nm2)

locBinOpTry :: BinOp -> BinOp -> Parser (LExpr -> LExpr -> LExpr)
locBinOpTry nm nm2 = do
  loc <- location
  opName <- mkLocated $ try (op nm) <|> op nm2
  return $ \le1 le2 ->
    Located loc $ EBinOp opName le1 le2

unOpExpr :: UnOp -> Operator Text () Identity LExpr
unOpExpr nm = Expr.Prefix (locUnOp nm)

locUnOp :: UnOp -> Parser (LExpr -> LExpr)
locUnOp nm = do
  loc <- location
  opName <- mkLocated $ unOp nm
  return $ \le ->
    Located loc $ EUnOp opName le

opTable :: OperatorTable Text () Identity LExpr
opTable =
  [ [ binOp RecordAccess Expr.AssocLeft ]
  , [ binOp Mul Expr.AssocLeft ]
  , [ binOp Add Expr.AssocLeft
    , binOp Sub Expr.AssocLeft
    , binOp Div Expr.AssocLeft
    ]
  , [ binOp And Expr.AssocLeft
    , binOp Or Expr.AssocLeft
    ]
  , [ binOp Equal Expr.AssocLeft
    , binOp NEqual Expr.AssocLeft
    , binOpTry LEqual Lesser Expr.AssocLeft
    , binOpTry GEqual Greater Expr.AssocLeft
    , binOp Greater Expr.AssocLeft
    ]
  , [ unOpExpr Not
    ]
  ]

op :: BinOp -> Parser BinOp
op oper = symbol (Lexer.opToken oper) >> pure oper
  <?> "binary operator"

unOp :: UnOp -> Parser UnOp
unOp oper = symbol (Lexer.unOpToken oper) >> pure oper
  <?> "unary operator"

expr :: Parser LExpr
expr = buildExpressionParser opTable locExpr
  where
    -- Expressions without locations and binary and unary ops.
    nonLocExpr :: Parser Expr
    nonLocExpr =  assignExpr
              <|> beforeExpr
              <|> afterExpr
              <|> betweenExpr
              <|> ifElseExpr
              <|> caseExpr
              <|> callExpr
              <|> litExpr
              <|> varExpr
              <|> constructorExpr
              <|> try mapExpr -- backtrack on failure and try parsing as set
              <|> setExpr
              <|> holeExpr

    -- Expressions without binary/unary operations or expressions with
    -- parentheses.
    locExpr :: Parser LExpr
    locExpr =  mkLocated nonLocExpr
           <|> parensLExpr

parensLExpr :: Parser LExpr
parensLExpr = parens expr

litExpr :: Parser Expr
litExpr = ELit <$> locLit
 <?> "literal"

varExpr :: Parser Expr
varExpr = EVar <$> locName
 <?> "variable"

constructorExpr :: Parser Expr
constructorExpr = EConstr <$> nameUpper <*> (parens (commaSep expr) <|> pure [])

assignExpr :: Parser Expr
assignExpr = do
  var <- try (nonEmptyUnsafe (name `sepEndBy1` char '.') <* reservedOp Token.assign)
  lexpr <- expr
  return $ EAssign var lexpr
 <?> "assign statement"

callExpr :: Parser Expr
callExpr = uncurry ECall <$> call

call :: Parser ((Either PrimOp LName), [LExpr])
call = do
  lnm@(Located _ nm) <-
    try $ Lexer.locName <* symbol Token.lparen
  let fname = case lookupPrim nm of
        Nothing  -> Right lnm
        Just pop -> Left pop
  args <- commaSep expr <* symbol Token.rparen
  return (fname, args)
    <?> "call statement"

ifElseExpr :: Parser Expr
ifElseExpr = do
  try $ reserved Token.if_
  cond <- parensLExpr
  e1 <- block
  e2 <- elseBranch
  return $ EIf cond e1 e2
 <?> "if statement"
  where
    -- Parse either an else block, or a noop expr
    elseBranch :: Parser LExpr
    elseBranch
      = (try (reserved Token.else_) *> (block <|> mkLocated ifElseExpr))
        <|> (mkLocated . pure) ENoOp
        <?> "else statement"

beforeExpr :: Parser Expr
beforeExpr = do
  try $ reserved Token.before
  dt <- parensLExpr
  e <- block
  return $ EBefore dt e
 <?> "before guard statement"

afterExpr :: Parser Expr
afterExpr = do
  try $ reserved Token.after
  dt <- parensLExpr
  e <- block
  return $ EAfter dt e
 <?> "after guard statement"

betweenExpr :: Parser Expr
betweenExpr = do
  try $ reserved Token.between
  start <- symbol Token.lparen *> expr
  lexeme comma
  end <- expr <* symbol Token.rparen
  e <- block
  return $ EBetween start end e
 <?> "between guard statement"

caseExpr :: Parser Expr
caseExpr = do
    _ <- try $ reserved Token.case_
    scrutinee <- expr
    _ <- symbol Token.lbrace
    matches <- match `sepEndBy1` semi
    _ <- symbol Token.rbrace
    pure $ ECase scrutinee matches
  where

    match = CaseBranch
      <$> mkLocated pattern
      <*  reserved Token.rarrow
      <*> (block <|> expr)

    pattern :: Parser Pattern
    pattern = foldr1 (<|>)
        [ PatLit <$> try locLit
        , PatVar <$> try Lexer.locName
        , PatConstr <$> try Lexer.locNameUpper <*> (parens (commaSep pattern) <|> pure [])
        , PatWildCard <$ (char '_' *> whiteSpace)
        ]

mapExpr :: Parser Expr
mapExpr =
  EMap . Map.fromList <$>
    parens (commaSep parseMapItem <* optional newline)
  where
    parseMapItem = do
      ek <- expr
      lexeme (symbol ":")
      ev <- expr
      optional newline
      pure (ek, ev)

setExpr :: Parser Expr
setExpr = ESet . Set.fromList <$> braces (commaSep expr <* optional newline)

holeExpr :: Parser Expr
holeExpr = EHole <$ reserved Token.hole

-- | Parses 0 or more expressions delimited by ';'
block :: Parser LExpr
block = (braces $ do
  loc <- location
  eseq loc <$> (expr `sepEndBy` semi))
 <?> "expression block"

-------------------------------------------------------------------------------
-- Transitions
-------------------------------------------------------------------------------

workflowPlaces :: Parser WorkflowState
workflowPlaces
  =   reserved Token.initial *> pure startState
  <|> reserved Token.terminal *> pure endState
  <|> makeWorkflowState <$> ((pure <$> Lexer.name) <|> braces (commaSep1 Lexer.name))
  <?> "workflow state"

transition :: Parser Transition
transition = do
  _ <- try $ reserved Token.transition
  Arrow <$> (workflowPlaces <* symbol Token.rarrow) <*> workflowPlaces
  <?> "transition"

-------------------------------------------------------------------------------
-- Algebraic data type definition
-------------------------------------------------------------------------------

adtDef :: Parser ADTDef
adtDef = do
    _ <- try (reserved Token.type_ <|> reserved Token.enum)
    tyName <- Lexer.locName
    -- when (locVal tyName `elem` Token.keywords)
    --   (parserFail )
    _ <- symbol Token.lbrace
    constructors <- nonEmptyUnsafe (constructor `sepEndBy1` semi)
    _ <- symbol Token.rbrace
    pure (ADTDef tyName constructors)
  where
    constructor = ADTConstr
      <$> Lexer.locNameUpper
      <*> (parens (commaSep namedType) <|> pure [])

    namedType = do
      ty <- type_
      _ <- whiteSpace
      nm <- locName
      pure (nm, ty)

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

script :: Parser Script
script = do
  adts <- many adtDef
  defns <- def `endBy` semi
  graph <- transition `endBy` semi
  methods <- many method
  helpers <- many helper
  return $ Script adts defns graph methods helpers
 <?> "script"

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

data ParseErrInfo = ParseErrInfo
  { line         :: Int
  , lineContents :: Text
  , lineAfterContents :: Text
  , column       :: Int
  , errMsg       :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON ParseErrInfo where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance FromJSON ParseErrInfo where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

mkParseErrInfo :: Text -> ParseError -> ParseErrInfo
mkParseErrInfo input perr = ParseErrInfo
  { line   = line
  , lineContents = T.unlines $ take 2 $ drop (line - 2) lines
  , lineAfterContents = T.unlines $ take 2 $ drop (line ) lines
  , column = column
  , errMsg = show perr
  }
  where
    line = sourceLine $ errorPos perr
    column = sourceColumn $ errorPos perr
    lines = T.lines input

instance Pretty ParseErrInfo where
  ppr ParseErrInfo{..} =
    text (toS lineContents)
    <> text (toS $ T.replicate (column - 1) "-") <> "^"
    <$$> text (toS lineAfterContents)
    <$$> (text $ toS errMsg )

-------------------------
-- Arbitrary
-------------------------

instance Arbitrary ParseErrInfo where
  arbitrary = ParseErrInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
