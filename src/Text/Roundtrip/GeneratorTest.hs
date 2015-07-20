import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO)
import Data.Text.Lazy
import Data.XML.Types

import Data.Enumerator
import Text.XML.Enumerator.Render

import qualified Data.Enumerator.List as DSL
import qualified Data.Text as DT

ns :: String -> Maybe String -> Maybe String -> Name
ns name namespace prefix = Name n ns p
        where
            n = pack name
            ns = liftM pack namespace
            p = liftM pack prefix

attr :: Name -> String -> Attribute
attr key value = Attribute key c
        where c = [ContentText $ pack value]

--renderEvents :: MonadIO m => [Event] -> m ()
renderEvents evs = run_ $ enumList 1 evs $$ joinI $ renderText $$ DSL.consume --printChunks False

sample0 = [EventBeginDocument, EventBeginElement (Name (Data.Text.Lazy.pack "foo") (Just (Data.Text.Lazy.pack "nm")) (Just (Data.Text.Lazy.pack "p"))) [Attribute (Name (Data.Text.Lazy.pack "x") (Just (Data.Text.Lazy.pack "nm")) Nothing) []]]

_SAMPLE1_ELEM1_NAME = ns "foo" (Just "urn:__foo") (Just "__foo")
_SAMPLE1_ELEM2_NAME = ns "bar" (Just "urn:foo") (Just "foo")
_SAMPLE1_ELEM3_NAME = (ns "spam" (Just "urn:bar") (Just "foo"))
_SAMPLE1_ELEM_EGG_NAME = ns "egg" Nothing Nothing

sample1 = [EventBeginDocument
          ,EventBeginElement _SAMPLE1_ELEM1_NAME
                [attr (ns "key" (Just "urn:_foo") (Just "_foo")) "value"
                ,attr (ns "key2" (Just "urn:_foo") (Just "_foo")) "value"]
          ,EventBeginElement _SAMPLE1_ELEM2_NAME
                [attr (ns "key" (Just "urn:_foo") (Just "_foo")) "value"]
          ,EventContent $ ContentText $ pack "BAR"
          ,EventEndElement _SAMPLE1_ELEM2_NAME
          ,EventBeginElement _SAMPLE1_ELEM2_NAME []
          ,EventBeginElement _SAMPLE1_ELEM3_NAME []
          ,EventBeginElement _SAMPLE1_ELEM_EGG_NAME []
          ,EventEndElement _SAMPLE1_ELEM_EGG_NAME
          ,EventContent $ ContentText $ pack "this is spam!"
          ,EventEndElement _SAMPLE1_ELEM3_NAME
          ,EventEndElement _SAMPLE1_ELEM2_NAME
          ,EventEndElement _SAMPLE1_ELEM1_NAME]

_SAMPLE2_ELEM_FOO_NAME = ns "foo" Nothing Nothing
_SAMPLE2_ELEM_BAR_NAME_NONS = ns "bar" Nothing Nothing
_SAMPLE2_ELEM_SPAM_NAME_NONS = ns "spam" Nothing Nothing
_SAMPLE2_ELEM_EGG_NAME_NONS = ns "egg" Nothing Nothing
_SAMPLE2_ELEM_BAR_NAME_NS = ns "bar" (Just "http://www.example.com") (Just "foo")
_SAMPLE2_ELEM_SPAM_NAME_NS = ns "spam" (Just "http://www.example.com") (Just "foo")
_SAMPLE2_ELEM_EGG_NAME_NS = ns "egg" (Just "http://www.example.com") (Just "foo")
_SAMPLE2_ELEM_HAM = ns "ham" Nothing Nothing

sample2 = [EventBeginDocument
          ,EventBeginElement _SAMPLE2_ELEM_FOO_NAME
            [attr (ns "key" Nothing Nothing) "value"
            ,attr (ns "key2" Nothing Nothing) "value2"]
          ,EventBeginElement _SAMPLE2_ELEM_BAR_NAME_NONS []
          ,EventEndElement _SAMPLE2_ELEM_BAR_NAME_NONS
          ,EventBeginElement _SAMPLE2_ELEM_SPAM_NAME_NONS
            [attr (ns "key" Nothing Nothing) "value"]
          ,EventEndElement _SAMPLE2_ELEM_SPAM_NAME_NONS
          ,EventBeginElement _SAMPLE2_ELEM_EGG_NAME_NONS []
          ,EventContent $ ContentText $ pack "ham"
          ,EventEndElement _SAMPLE2_ELEM_EGG_NAME_NONS

          ,EventBeginElement _SAMPLE2_ELEM_BAR_NAME_NS []
          ,EventEndElement _SAMPLE2_ELEM_BAR_NAME_NS
          ,EventBeginElement _SAMPLE2_ELEM_SPAM_NAME_NS
            [attr (ns "key" (Just "http://www.example.com") (Just "foo")) "value"]
          ,EventEndElement _SAMPLE2_ELEM_SPAM_NAME_NS
          ,EventBeginElement _SAMPLE2_ELEM_EGG_NAME_NS []
          ,EventBeginElement _SAMPLE2_ELEM_HAM []
          ,EventEndElement _SAMPLE2_ELEM_HAM
          ,EventEndElement _SAMPLE2_ELEM_EGG_NAME_NS

          ,EventEndElement _SAMPLE2_ELEM_FOO_NAME]

_SAMPLE3_ELEM_FOO_NAME = ns "foo" Nothing Nothing

sample3 = [EventBeginDocument
          ,EventBeginElement _SAMPLE3_ELEM_FOO_NAME [attr (ns "key" Nothing Nothing) "val\"'&<>ue"]
          ,EventContent $ ContentText $ pack "<&;'"
          ,EventEndElement _SAMPLE3_ELEM_FOO_NAME
          ,EventEndDocument]

main =
    do events <- renderEvents sample3
       putStrLn $ DT.unpack $ Prelude.head $ events
