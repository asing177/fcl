{-|

The pretty printer for FCL syntax, types and values.

-}

{-# LANGUAGE TypeApplications #-}

module Language.FCL.Pretty (
  Doc,
  Pretty(..),

  -- ** Printing
  Language.FCL.Pretty.print,
  render,
  pprRender,
  prettyPrint,
  printList,

  -- ** Leijen.Text exports
  (<>),
  (PP.<+>),
  (PP.<$$>),
  (PP.<//>),
  PP.dquotes,
  PP.hang,
  PP.hcat,
  PP.hsep,
  PP.indent,
  PP.lbrace,
  PP.line,
  PP.linebreak,
  PP.nest,
  PP.rbrace,
  PP.sep,
  PP.softbreak,
  PP.squotes,
  PP.stringStrict,
  PP.text,
  PP.vcat,
  PP.vsep,
  PP.angles,

  -- ** Utils
  (<$$+>),
  (<$$$>),
  (<$$$+>),
  append,
  listOf,
  tupleOf,
  setOf,
  bracketList,
  commafy,
  semify,
  parensIf,
  parens,
  ppMaybe,
  ppshow,
  assign,
  token,
  spaced,
  punctuate,
  intersperse,
  sqppr,
  panicppr,

  -- ** Testing
  testPpr,
  testPprList,
) where

import Protolude hiding ((<>), (<$>))
import Text.PrettyPrint.Leijen.Text as PP hiding (Pretty, equals)
import Data.Monoid ((<>))

import qualified Data.Map as Map

import qualified Language.FCL.Token as Token
import Language.FCL.Utils ((?))

pprRender :: Pretty a => a -> LText
pprRender = render . ppr

prettyPrint :: Pretty a => a -> Text
prettyPrint = toS . pprRender

-------------------------------------------------------------------------------
-- Base Class
-------------------------------------------------------------------------------

class Pretty p where
  ppr :: p -> Doc
  {-# MINIMAL ppr #-}

instance Pretty Text where
  ppr = text . fromStrict

instance Pretty LText where
  ppr = text

instance Pretty ByteString where
  ppr = text . toS

instance Pretty [Char] where
  ppr = text . toS

instance Pretty Int where
  ppr = int

instance Pretty Int64 where
  ppr = int . fromIntegral

instance Pretty Integer where
  ppr = integer

instance Pretty Rational where
  ppr r = ppr n <> (d /= 1 ? ppr @Text " / " <> ppr d)
    where
      n = numerator r
      d = denominator r

instance Pretty Double where
  ppr = double

instance Pretty Float where
  ppr = float

instance Pretty Bool where
  ppr = PP.bool

instance Pretty Doc where
  ppr = identity

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  ppr (Left a)  = ppr a
  ppr (Right b) = ppr b

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  ppr m = lbrace
    <$> nest 3 (vsep (map (\(k,v) -> (ppr k <+> ":" <+> ppr v)) (Map.toList m)))
    <$> rbrace

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

(<$$+>) :: Doc -> Doc -> Doc
d1 <$$+> d2 = d1 <$$> indent 3 d2

(<$$$>) :: Doc -> Doc -> Doc
d1 <$$$> d2 = d1 <> linebreak <> linebreak <> d2

(<$$$+>) :: Doc -> Doc -> Doc
d1 <$$$+> d2 = d1 <> linebreak <> linebreak <> indent 3 d2

listOf :: Pretty a => [a] -> Doc
listOf xs = token Token.lbrace <> commafy (fmap ppr xs) <> token Token.rbrace

tupleOf :: Pretty a => [a] -> Doc
tupleOf xs = token Token.lparen <> commafy (fmap ppr xs) <> token Token.rparen

-- TODO Very similar to 'listOf' :/
setOf :: (Foldable f, Pretty a) => f a -> Doc
setOf = encloseSep "{" "}" "," . map ppr . toList

-- TODO Very similar to 'listOf' :/
bracketList :: (Foldable f, Pretty a) => f a -> Doc
bracketList = encloseSep "[" "]" "," . map ppr . toList

commafy :: [Doc] -> Doc
commafy = hsep . punctuate comma

append :: Doc -> Doc -> Doc
append = flip (<>)

semify :: Doc -> Doc
semify = flip (<>) semi

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = identity

sqppr :: Pretty a => a -> Doc
sqppr = squotes . ppr

ppMaybe :: Pretty a => Maybe a -> Doc
ppMaybe = maybe mempty ppr

ppshow :: Show a => a -> Doc
ppshow x = ppr (show x :: Text)

assign :: Doc -> Doc -> Doc
assign doc1 doc2 = doc1 <+> token Token.assign <+> doc2 <> semi

-- | Put soft line breaks between elements.
spaced :: [Doc] -> [Doc]
spaced = intersperse softbreak

token :: Text -> Doc
token = text . fromStrict

render :: Doc -> LText
render = displayT . renderPretty 1 120

print :: Pretty a => a -> LText
print = render . ppr

printList :: Pretty a => [a] -> LText
printList xs = render (vcat (fmap ppr xs))


-- | Pretty print to stdout. Declare @default (Text)@ in the file if you get
-- an "ambiguous type variable" error.
putppr :: Pretty a => a -> IO ()
putppr = putText . prettyPrint

panicppr :: Pretty a => a -> b
panicppr = panic . prettyPrint

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

testPpr :: Pretty a => a -> IO ()
testPpr = putStrLn . render . ppr

testPprList :: LText
testPprList = render $ ppr $ listOf [True, False, True]
