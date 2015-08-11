module Text.Roundtrip.Classes where

import Prelude hiding ((<*>), pure)

import Data.Eq (Eq)
import Data.Char (Char)

import Data.Text as T
import Data.XML.Types (Name(..), Content)

import Control.Isomorphism.Partial (IsoFunctor)

infixl 3 <|>
infixl 3 <||>
infixr 6 <*>

class ProductFunctor f where
  (<*>) :: f alpha -> f beta -> f (alpha, beta)

class Alternative f where
  -- one token lookahead for lhs
  (<|>)  :: f alpha -> f alpha -> f alpha
  x <|> y = x <||> y
  -- infinite lookahead for lhs
  (<||>) :: f alpha -> f alpha -> f alpha
  empty  :: f alpha

class (IsoFunctor delta, ProductFunctor delta, Alternative delta)
   => Syntax delta where
  -- (<$>)   ::  Iso alpha beta -> delta alpha -> delta beta
  -- (<*>)   ::  delta alpha -> delta beta -> delta (alpha, beta)
  -- (<|>)   ::  delta alpha -> delta alpha -> delta alpha
  -- empty   ::  delta alpha
  pure    ::  Eq alpha => alpha -> delta alpha
  rule :: String -> delta beta -> delta alpha -> delta alpha
  rule _ _ x = x
  ruleInfix :: String -> delta beta -> delta gamma -> delta alpha -> delta alpha
  ruleInfix _ _ _ x = x

class Syntax delta => StringSyntax delta where
  token :: (Char -> Bool) -> delta Char
  anyToken :: delta Char
  anyToken = token (const True)

type Attribute = (Name, [Content])

class Syntax delta => XmlSyntax delta where
    xmlBeginDoc :: delta ()
    xmlEndDoc :: delta ()
    xmlBeginElem :: Name -> delta ()
    xmlEndElem :: Name -> delta ()
    xmlAttrValue :: Name -> delta T.Text -- FIXME: parser for attr value
    xmlTextNotEmpty :: delta T.Text
