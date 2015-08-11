{-# LANGUAGE BangPatterns #-}
module Text.Roundtrip.Combinators
  (  -- * Lexemes
     char
   , char'
  ,  string
  ,  comma
  ,  dot
     -- * Repetition
  ,  many
  ,  many1
  ,  sepBy
  ,  chainl1
     -- * Sequencing
  ,  (*>)
  ,  (<*)
  ,  between
     -- * Alternation
  ,  (<+>)
  ,  optional
  ,  optionalBool
  ,  optionalWithDefault
     -- * Whitespace
  ,  skipSpace
  ,  sepSpace
  ,  optSpace
     -- * XML
  , xmlEatWhiteSpace
  , xmlElem
  , xmlAttr
  , xmlFixedAttr
  , xmlText
  , xmlString
) where

import Prelude hiding (pure, (*>), (<*), (<*>), (<$>), (.), foldl)

import Control.Category ((.))

import Data.Char (isSpace)

import qualified Data.Text as T

import Data.XML.Types (Name)

import Control.Isomorphism.Partial

import Text.Roundtrip.Classes

-- derived combinators
many :: Syntax delta => delta alpha -> delta [alpha]
many p =
    rule "many" p $
    (cons <$> p <*> many p
     <|> nil <$> pure ())

many1 :: Syntax delta => delta alpha -> delta [alpha]
many1 p =
    rule "many1" p $ cons <$> p <*> many p

infixl 4 <+>

(<+>) :: Syntax delta => delta alpha -> delta beta -> delta (Either alpha beta)
p <+> q = (left <$> p) <|> (right <$> q)

char :: StringSyntax delta => Char -> delta ()
char c = ignore c <$> token (c ==)

char' :: StringSyntax delta => Char -> delta Char
char' c = token (c ==)

-- | `string` parses\/prints a fixed text and consumes\/produces a unit value.
string :: StringSyntax delta => String -> delta ()
string []      =    pure ()
string (c:cs)  =    inverse (element ((), ()))
               <$>  char c <*>  string cs

infixr 6 *>, <*

-- | This variant of `<*>` ignores its left result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost.

(*>) :: Syntax delta => delta () -> delta alpha -> delta alpha
p *> q = ruleInfix "*>" p q $ inverse unit . commute <$> p <*> q

-- | This variant of `<*>` ignores its right result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost.

(<*) :: Syntax delta => delta alpha -> delta () -> delta alpha
p <* q = ruleInfix "<*" p q $ inverse unit <$> p <*> q

-- | The `between` function combines `*>` and `<*` in the obvious way.
between :: Syntax delta => delta () -> delta () -> delta alpha -> delta alpha
between p q r = p *> r <* q

-- | The `chainl1` combinator is used to parse a
-- left-associative chain of infix operators.
chainl1 :: Syntax delta => delta alpha -> delta beta -> Iso (alpha, (beta, alpha)) alpha -> delta alpha
chainl1 arg op f
  = foldl f <$> arg <*> many (op <*> arg)

optional :: Syntax delta => delta alpha -> delta (Maybe alpha)
optional x  = rule "optional" x $ just <$> x <|> nothing <$> (pure ())

optionalBool :: Syntax delta => delta () -> delta Bool
optionalBool x  = maybeUnitBoolIso <$> optional x

optionalWithDefault :: (Eq alpha, Syntax delta) => alpha -> delta alpha -> delta alpha
optionalWithDefault def x = rule "optionalWithDefault" x $ x <|> pure def

sepBy :: Syntax delta => delta alpha -> delta () -> delta [alpha]
sepBy x sep
  =   cons <$> x <*> many (sep *> x)
  <|> nil <$> (pure ())

comma :: StringSyntax delta => delta ()
comma = char ','

dot :: StringSyntax delta => delta ()
dot = char '.'


-- Expressing whitespace
-- ---------------------
--
-- Parsers and pretty printers treat whitespace
-- differently. Parsers
-- specify where whitespace is allowed or required to occur, while
-- pretty printers specify how much whitespace is to be inserted at
-- these locations. To account for these different roles of
-- whitespace, the following three syntax descriptions provide
-- fine-grained control over where whitespace is allowed, desired or
-- required to occur.

-- | `skipSpace` marks a position where whitespace is allowed to
-- occur. It accepts arbitrary space while parsing, and produces
-- no space while printing.

skipSpace  ::  StringSyntax delta => delta ()
skipSpace  =   ignore []    <$>  many (char ' ')

-- | `optSpace` marks a position where whitespace is desired to occur.
-- It accepts arbitrary space while parsing, and produces a
-- single space character while printing.

optSpace  ::  StringSyntax delta => delta ()
optSpace  =   ignore [()]  <$>  many (char ' ')

-- | `sepSpace` marks a position where whitespace is required to
-- occur. It requires one or more space characters while parsing,
-- and produces a single space character while printing.

sepSpace  ::  StringSyntax delta => delta ()
sepSpace  =   char ' ' <* skipSpace

--
-- XML
--
xmlEatWhiteSpace :: XmlSyntax d => d ()
xmlEatWhiteSpace = ignore T.empty . namedSubset "allIsSpace" (T.all isSpace) <$> xmlText <|> pure ()

xmlElem :: XmlSyntax x => Name -> x a -> x a
xmlElem name children =
    xmlBeginElem name *> children <* xmlEndElem name

xmlAttr :: XmlSyntax x => Name -> Iso T.Text a -> x a
xmlAttr name p = p <$> xmlAttrValue name

xmlFixedAttr :: XmlSyntax x => Name -> T.Text -> x ()
xmlFixedAttr name value = fixedValue value <$> xmlAttrValue name

xmlText :: XmlSyntax d => d T.Text
xmlText = optionalWithDefault T.empty xmlTextNotEmpty

xmlString :: XmlSyntax d => d String
xmlString = textStringIso <$> xmlText
