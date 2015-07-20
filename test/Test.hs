{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import System.Environment
import qualified Data.Text as T

import Data.Enumerator (joinI, ($$), run_)
import Text.XML.Enumerator.Parse (parseBytes)
import Data.Enumerator.Binary (enumFile)

import Text.Roundtrip
import Text.Roundtrip.Xml
import Text.Roundtrip.Xml.Enumerator
import Control.Isomorphism.Partial.TH

type Text = T.Text

data NewsList = NewsList [NewsItem]
              deriving (Show)

data NewsItem = NewsItem Int Text
              deriving (Show)

$(defineIsomorphisms ''NewsList)
$(defineIsomorphisms ''NewsItem)

newsListSpec :: XmlSyntax x => x NewsList
newsListSpec = newsList <$> xmlElem "news" (many newsItemSpec)

newsItemSpec :: XmlSyntax x => x NewsItem
newsItemSpec =
    newsItem <$> xmlElem "newsitem" (xmlAttr "seq" readShowIso <*>
                                     xmlAttr "object" idIso)

main :: IO ()
main =
    do args <- getArgs
       x <- parseFromFile (args!!0) newsListSpec
       print x

parseFromFile fname p =
    do run_ $ joinI $
              enumFile fname $$
              parseBytes $$
              parseXml fname defaultEntityRenderer p
