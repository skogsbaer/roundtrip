{-# LANGUAGE ScopedTypeVariables #-}

module Text.Roundtrip.SpecPrinter (

  SpecPrinter, specPrinter, runSpecPrinter

) where

import Prelude hiding (catch, (<>))

import Control.Exception (AsyncException, catch)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.HughesPJ
import Data.XML.Types (Name(..), Content)

import Control.Isomorphism.Partial
import Text.Roundtrip hiding ((<+>))

newtype SpecPrinter a = SpecPrinter { unSpecPrinter :: Doc }

specPrinter :: Doc -> SpecPrinter a
specPrinter = SpecPrinter

instance IsoFunctor SpecPrinter where
    iso <$> (SpecPrinter p) = SpecPrinter $ text (isoName iso) <+> text "<$>" <+> p

instance ProductFunctor SpecPrinter where
    (SpecPrinter p) <*> (SpecPrinter q) =
        SpecPrinter $ parens (p <+> text "<*>" <+> q)

instance Alternative SpecPrinter where
    (SpecPrinter p) <|> (SpecPrinter q) =
        SpecPrinter $ parens (p <+> text "<|>" <+> q)
    (SpecPrinter p) <||> (SpecPrinter q) =
        SpecPrinter $ parens (p <+> text "<||>" <+> q)
    empty = SpecPrinter $ text "empty"

instance Syntax SpecPrinter where
    pure _ = SpecPrinter $ text "pure"
    rule name (SpecPrinter p) _ = SpecPrinter $ text name <+> p
    ruleInfix name (SpecPrinter p) (SpecPrinter q) _ = SpecPrinter $ p <+> text name <+> q

instance XmlSyntax SpecPrinter where
    xmlBeginDoc = specPrinter $ text "begin-doc"
    xmlEndDoc = specPrinter $ text "end-doc"
    xmlBeginElem name = specPrinter $ text "<" <> text (formatName name) <+> text "...>"
    xmlEndElem name = specPrinter $ text "</" <> text (formatName name) <> text ">"
    xmlAttrValue name = specPrinter $ text "attr" <+> text (formatName name)
    xmlTextNotEmpty = specPrinter $ text "text-node"

formatName :: Name -> String
formatName n =
    case n of
      Name localName _ (Just prefix) -> T.unpack prefix ++ ':' : T.unpack localName
      Name localName (Just ns) Nothing -> '{' : (T.unpack ns ++ '}' : T.unpack localName)
      Name localName Nothing Nothing -> T.unpack localName

runSpecPrinter :: SpecPrinter a -> String
runSpecPrinter (SpecPrinter p) = render p
