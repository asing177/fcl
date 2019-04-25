{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|

Parser for the FCL scripting language.

-}

module Script.Parser (
  -- ** Parser
  parseExpr,
  parseScript,
  parseText,
  parseFile,

  parseDefn,
  parseLit,
  parseType,
  parseFixedN,
  parseTimeDelta,
  parseDateTime,
  parseWorkflowState,
  parseBlock,
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

  fixedLit,
  rawTextLit,
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

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr as Expr
import Text.ParserCombinators.Parsec.Error
import qualified Text.Parsec.Token as Tok

import Data.Aeson (ToJSON(..), FromJSON)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (digitToInt)
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Fixed
import Script hiding (mapType)
import Address
import Script.Lexer as Lexer
import Script.Pretty hiding (parens)
import Script.Prim (lookupPrim)
import qualified SafeString as SS
import qualified Script.Token as Token
import qualified Datetime.Types as DT

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- | Parse an expression.
parseExpr :: T.Text -> Either ParseErrInfo LExpr
parseExpr input = first (mkParseErrInfo input)
  $ parse (contents expr) "<stdin>" input

-- | Parse file contents into a script.
parseScript :: T.Text -> Either ParseErrInfo Script
parseScript input = first (mkParseErrInfo input)
  $ parse (contents script <* eof) "<stdin>" input

-- | Parse text not expecting eof
parseText :: Text -> Either ParseErrInfo Script
parseText input = first (mkParseErrInfo input)
  $ parse (contents script) mempty input

-- | Parse a file into a script.
parseFile :: FilePath -> IO Script
parseFile path = do
  eScript <- parseScript <$> readFile path
  either panicppr pure eScript

parseDefn :: Text -> Either ParseErrInfo Def
parseDefn input = first (mkParseErrInfo input)
  $ parse (contents def) "definition" input

parseMethod :: Text -> Either ParseErrInfo Method
parseMethod input = first (mkParseErrInfo input)
  $ parse (contents method) "method" input

parseLit :: Text -> Either ParseErrInfo Lit
parseLit input = first (mkParseErrInfo input)
  $ parse (contents lit) "literal" input

parseType :: Text -> Either ParseErrInfo Type
parseType input = first (mkParseErrInfo input)
  $ parse (contents type_) "type" input

parseFixedN :: Text -> Either ParseErrInfo FixedN
parseFixedN input = first (mkParseErrInfo input)
  $ parse (contents fixedN) "fixedN" input

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

contents :: Parser a -> Parser a
contents p = whiteSpace *> p

testParse :: T.Text -> IO ()
testParse = parseTest (contents script)


-------------------------------------------------------------------------------
-- Lit
-------------------------------------------------------------------------------


lit :: Parser Lit
lit =  fixedLit
   <|> floatLit
   <|> timedeltaLit
   <|> int64Lit
   <|> boolLit
   <|> addressLit
   <|> stateLit
   <|> datetimeLit
   <|> textLit
   <|> voidLit
   <|> enumConstrLit
   <?> "literal"

locLit :: Parser LLit
locLit = mkLocated lit

maxi64 :: Integer
maxi64 = fromIntegral (maxBound :: Int64)

mini64 :: Integer
mini64 = fromIntegral (minBound :: Int64)

int64Lit :: Parser Lit
int64Lit = do
  n <- Tok.integer lexer
  if n > maxi64
    then parserFail "Integer overflows int64"
    else if n < mini64
      then parserFail "Integer underflows int64"
      else pure (LInt (fromIntegral n))

floatLit :: Parser Lit
floatLit = try $ do
  mNeg <- optionMaybe $ char '-'
  LFloat <$> case mNeg of
    Nothing -> Tok.float lexer
    Just neg -> negate <$> try (Tok.float lexer)
   <?> "float literal"

fixedNLit :: PrecN -> Parser FixedN
fixedNLit precn = do
  let prec = fromEnum precn + 1
  try $ do
    -- Parse the left hand side of the decimal point
    sign <- maybe 1 (const (-1)) <$> optionMaybe (char '-')
    lhs <- Tok.integer lexer <* char '.'

    -- Parse the rhs, prec # of digits ending in 'f'
    rhs' <- count prec digit <* lexeme (char 'f')
    let rhs = map (toInteger . digitToInt) rhs'

    -- Convert the rhs to an integer
    let decs = sum $ zipWith (\n e -> n*(10^e)) (reverse rhs) [0..]

    -- Turn the lhs and rhs into a fixed point
    pure $ mkFixed precn $ sign * (lhs*(10^prec) + decs)

fixedN :: Parser FixedN
fixedN =  fixedNLit Prec6
      <|> fixedNLit Prec5
      <|> fixedNLit Prec4
      <|> fixedNLit Prec3
      <|> fixedNLit Prec2
      <|> fixedNLit Prec1
      <?> "fixed point number with 1-6 decimal places, ending in an 'f'"

fixedLit :: Parser Lit
fixedLit = LFixed <$> fixedN

boolLit :: Parser Lit
boolLit = (fmap LBool
  $  False <$ try (reserved Token.false)
 <|> True  <$ try (reserved Token.true))
 <?> "boolean literal"

rawAddress :: Parser (Address a)
rawAddress
  = Address.fromBS . BS8.pack <$> between (symbol "\'") (symbol "\'") (many1 alphaNum)

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
textLit = LText . SS.fromBytes' . BS8.pack <$> rawTextLit
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

voidLit :: Parser Lit
voidLit = LVoid <$ try (reserved Token.void)
 <?> "void literal"

enumConstrLit :: Parser Lit
enumConstrLit = LConstr <$> try (symbol "`" *> Lexer.enumConstr)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type_ :: Parser Type
type_ =  intType
     <|> floatType
     <|> fixedType
     <|> boolType
     <|> voidType
     <|> accountType
     <|> assetType
     <|> contractType
     <|> sigType
     <|> textType
     <|> dateType
     <|> timedeltaType
     <|> enumType
     <|> collectionType
     <?> "type"

intType :: Parser Type
intType = TInt <$ try (reserved Token.int)

floatType :: Parser Type
floatType = TFloat <$ try (reserved Token.float)

fixedType :: Parser Type
fixedType = do
    prec <- prec1
        <|> prec2
        <|> prec3
        <|> prec4
        <|> prec5
        <|> prec6
    pure $ TFixed prec
  where
    prec1 = Prec1 <$ try (reserved Token.fixed1)
    prec2 = Prec2 <$ try (reserved Token.fixed2)
    prec3 = Prec3 <$ try (reserved Token.fixed3)
    prec4 = Prec4 <$ try (reserved Token.fixed4)
    prec5 = Prec5 <$ try (reserved Token.fixed5)
    prec6 = Prec6 <$ try (reserved Token.fixed6)

boolType :: Parser Type
boolType = TBool <$ try (reserved Token.bool)

voidType :: Parser Type
voidType = TVoid <$ try (reserved Token.void)

accountType :: Parser Type
accountType = TAccount <$ try (reserved Token.account)

assetType :: Parser Type
assetType = do
    atype <- parseAssetBinary
         <|> parseAssetDiscrete
         <|> parseAssetFrac
    pure $ TAsset atype
  where
    parseAssetBinary   = TBinary <$ try (reserved Token.assetBin)
    parseAssetDiscrete = TDiscrete <$ try (reserved Token.assetDis)
    parseAssetFrac     =
      fmap TFractional $
            (Prec1 <$ try (reserved Token.assetFrac1))
        <|> (Prec2 <$ try (reserved Token.assetFrac2))
        <|> (Prec3 <$ try (reserved Token.assetFrac3))
        <|> (Prec4 <$ try (reserved Token.assetFrac4))
        <|> (Prec5 <$ try (reserved Token.assetFrac5))
        <|> (Prec6 <$ try (reserved Token.assetFrac6))

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

enumType :: Parser Type
enumType = TEnum <$> try (reserved Token.enum *> Lexer.name)

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
def = try globalDef
  <|> globalDefNull
  <?> "definition"

globalDef :: Parser Def
globalDef = do
  optional (reserved Token.global <|> reserved Token.local)
  typ <- type_
  precs <- preconditions <|> pure (mempty @Preconditions)
  id <- name
  reservedOp Token.assign
  lexpr <- expr
  return $ GlobalDef typ precs id lexpr
 <?> "global definition"

globalDefNull :: Parser Def
globalDefNull = do
  optional (reserved Token.global <|> reserved Token.local)
  typ <- type_
  precs <- preconditions <|> pure (mempty @Preconditions)
  Located loc id <- locName
  return $ GlobalDefNull typ precs (Located loc id)
 <?> "global definition"

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
  [ [ binOp Mul Expr.AssocLeft ]
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

assignExpr :: Parser Expr
assignExpr = do
  var <- try $ name <* reservedOp Token.assign
  lexpr <- expr
  return $ EAssign var lexpr
 <?> "assign statement"

callExpr :: Parser Expr
callExpr = do
  lnm@(Located _ nm) <-
    try $ Lexer.locName <* symbol Token.lparen
  let fname = case lookupPrim nm of
        Nothing  -> Right lnm
        Just pop -> Left pop
  args <- commaSep expr <* symbol Token.rparen
  return $ ECall fname args
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
    elseBranch = do
      loc <- location
      Text.Parsec.option
        (Located loc $ ENoOp)
        (try (reserved Token.else_) *> block
          <?> "else statement")

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
    try $ reserved Token.case_
    scrutinee <- parensLExpr
    symbol Token.lbrace
    matches <- many1 (Match <$> pattern_
                            <* reserved Token.rarrow
                            <*> (block <|> expr)
                            <* semi)
    symbol Token.rbrace
    return $ ECase scrutinee matches
  where
    pattern_ :: Parser LPattern
    pattern_ = do
      loc <- location
      Located loc . PatLit <$> try (symbol "`" *> Lexer.enumConstr)

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
  <|> do
    places <- (pure <$> Lexer.name) <|> braces (commaSep1 Lexer.name)
    case makeWorkflowState places of
      Right wfst -> pure wfst
      Left err -> parserFail $ show err
  <?> "workflow state"

transition :: Parser Transition
transition = do
  reserved Token.transition
  Arrow <$> (workflowPlaces <* symbol Token.rarrow) <*> workflowPlaces
  <?> "transition"

-------------------------------------------------------------------------------
-- Enumeration type definition
-------------------------------------------------------------------------------

enumDef :: Parser EnumDef
enumDef = do
  reserved Token.enum
  lname <- Lexer.locName
  constrs <- braces $ commaSep1 Lexer.locEnumConstr
  return $ EnumDef lname constrs

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

script :: Parser Script
script = do
  enums <- endBy enumDef semi
  defns <- endBy def semi
  graph <- endBy transition semi
  methods <- many method
  helpers <- many helper
  return $ Script enums defns graph methods helpers
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
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

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
