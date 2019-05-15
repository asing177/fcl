{-|

Lexer for the FCL scripting language.

-}

module Language.FCL.Lexer (
  -- ** Lexer
  lexer,
  lexeme,

  -- ** Lexemes
  identifier,
  name,
  enumConstr,
  locEnumConstr,
  locName,
  opToken,
  unOpToken,
  whiteSpace,
  reserved,
  reservedOp,
  parens,
  braces,
  brackets,
  semi,
  comma,
  commaSep,
  commaSep1,
  semiSep,
  semiSep1,
  symbol,

  -- ** Position
  location,
  mkLocated,
) where

import Protolude hiding ((<|>), bool, many, try, optional, sourceLine, sourceColumn)

import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.Functor.Identity (Identity)
import qualified Data.Text as T

import Language.FCL.AST (Name(..), Loc(..), Located(..), LName, BinOp(..), UnOp(..), EnumConstr(..), LEnumConstr)
import qualified Language.FCL.Token as Token

import qualified Data.Text.Encoding as Text

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
  { Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = Tok.opLetter style
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedOpNames = fmap T.unpack Token.operators
  , Tok.reservedNames   = fmap T.unpack Token.keywords
  , Tok.caseSensitive   = True
  }

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

identifier :: Parser T.Text
identifier = T.pack <$> Tok.identifier lexer
  <?> "identifier"

name :: Parser Name
name = Name <$> identifier
  <?> "name"

locName :: Parser LName
locName = mkLocated name

enumConstr :: Parser EnumConstr
enumConstr = EnumConstr <$> identifier
  <?> "enum constructor"

locEnumConstr :: Parser LEnumConstr
locEnumConstr = mkLocated enumConstr

unOpToken :: UnOp -> T.Text
unOpToken Not = Token.not

opToken :: BinOp -> T.Text
opToken o = case o of
  Add     -> Token.add
  Mul     -> Token.mult
  Sub     -> Token.sub
  Div     -> Token.div
  And     -> Token.and
  Or      -> Token.or
  Equal   -> Token.equal
  NEqual  -> Token.nequal
  GEqual  -> Token.gequal
  LEqual  -> Token.lequal
  Lesser  -> Token.lesser
  Greater -> Token.greater

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer
  <?> "whitespace"

reserved :: T.Text -> Parser ()
reserved nm = Tok.reserved lexer (T.unpack nm)
  <?> "reserved name"

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer (T.unpack op)
  <?> "reserved operator"

parens :: Parser a -> Parser a
parens x = Tok.parens lexer x
  <?> "parens"

braces :: Parser a -> Parser a
braces x = Tok.braces lexer x
  <?> "braces {}"

brackets :: Parser a -> Parser a
brackets x = Tok.brackets lexer x
  <?> "brackets"

semi :: Parser [Char]
semi = Tok.semi lexer
  <?> "semicolon"

comma :: Parser [Char]
comma = Tok.comma lexer
  <?> "comma"

commaSep :: Parser a -> Parser [a]
commaSep xs = Tok.commaSep lexer xs
  <?> "comma seperated list"

commaSep1 :: Parser a -> Parser [a]
commaSep1 xs = Tok.commaSep1 lexer xs
  <?> "non-empty comma seperated list"

semiSep :: Parser a -> Parser [a]
semiSep xs = Tok.semiSep lexer xs
  <?> "semicolon seperated list"

semiSep1 :: Parser a -> Parser [a]
semiSep1 xs = Tok.semiSep1 lexer xs
  <?> "semicolon seperated list"

symbol :: T.Text -> Parser T.Text
symbol = fmap T.pack . Tok.symbol lexer . T.unpack

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

location :: Parser Loc
location = do
  srcPos <- getPosition
  let line = sourceLine srcPos
      col  = sourceColumn srcPos
  pure $ Loc line col

mkLocated :: Parser a -> Parser (Located a)
mkLocated p = Located <$> location <*> p
