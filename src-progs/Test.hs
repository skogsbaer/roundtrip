{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeSynonymInstances, KindSignatures, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}
-- module Main where

import Prelude hiding (elem, (.))

import Control.Category ((.))
import System.Environment
import System.IO
import qualified Data.Map as Map
import Data.XML.Types (Name(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Word
import Data.Char (toUpper)
import Data.List (isInfixOf)
import qualified Data.List as List
import Data.Enumerator (joinI, ($$), run_, enumList, printChunks, isEOF)
import Data.Enumerator.List (consume)
import Data.Enumerator.Binary (enumFile, iterHandle)
import Text.XML.Enumerator.Parse (parseBytes)
import Text.XML.Enumerator.Render (renderBytes)

import qualified Codec.Binary.Base64 as Base64

import Text.Roundtrip
import Text.Roundtrip.SpecPrinter
import Text.Roundtrip.Xml
import Text.Roundtrip.Xml.Enumerator
import Control.Isomorphism.Partial.TH

import qualified Data.ByteString.Lazy as BSL

import MobileGateway.Util.ConClasses
import MobileGateway.Model.CoreTypes
import MobileGateway.Model.DocTypes hiding (Name)
import qualified MobileGateway.Model.DocTypes as DocTypes
import MobileGateway.Model.WithObjType (HasObjType, WithObjType(..), AnyRefObj, wrapWot, unwrapWot)
import MobileGateway.Model.XsdTypes (parseXsdDate, formatXsdDate,
                                     parseXsdDateTime, formatXsdDateTime)

class XmlPickler a where
    xpickle :: XmlSyntax d => d a

class XmlPickler' t where
    xpickle' :: (Show a, XmlPickler a, XmlSyntax d) => d (t a)

{-
instance (Show' k, Show' l, XmlPickler' k, XmlPickler' l) => XmlPickler' (Either' k l) where
    xpickle' =
        xpTagSwitch (takeWhile (/= ' ') . show')
        [ xpCase (Either' . Left, \(Either' (Left x)) -> x) xpickle'
        , xpCase (Either' . Right, \(Either' (Right x)) -> x) xpickle'
        ]
-}

data RefKind = UseRef | DefRef

class Show' r => RefPickler r where
    pickleRef :: (Show (t r'), XmlPickler (t r'), XmlSyntax d, HasObjType t, RefPickler r')
              => RefKind -> d (r (t r'))

{-
instance (Show' r1, Show' r2, RefPickler r1, RefPickler r2) => RefPickler (Either' r1 r2) where
    pickleRef kind =
        xpTagSwitch (takeWhile (/= ' ') . show')
        [ xpCase (Either' . Left, \(Either' (Left x)) -> x) (pickleRef kind)
        , xpCase (Either' . Right, \(Either' (Right x)) -> x) (pickleRef kind)
        ]
-}

_DEFNS_ = "http://www.factisresearch.com/ns/sync"
_CPNS_ = "http://www.factisresearch.com/ns/checkpad"
_IDOCNS_ = "http://www.factisresearch.com/ns/idoc"

elemSync :: XmlSyntax x => TL.Text -> x a -> x a
elemSync n = xmlElem $ Name n (Just _DEFNS_) (Just "sync")

elemCp :: XmlSyntax x => TL.Text -> x a -> x a
elemCp n = xmlElem $ Name n (Just _CPNS_) (Just "cp")

elemIDoc :: XmlSyntax x => TL.Text -> x a -> x a
elemIDoc n = xmlElem $ Name n (Just _IDOCNS_) (Just "idoc")


-- ---------------------------------------------------------------------------
--  ObjId, ObjRef, ObjLink
-- ---------------------------------------------------------------------------
instance XmlPickler' ObjId where
    xpickle' = xpObjId

xpObjId :: XmlSyntax d => d (ObjId a)
xpObjId = readShowIso <$> xmlText

-- ---------------------------------------------------------------------------
--  ObjRef, ObjLink
-- ---------------------------------------------------------------------------

instance RefPickler Obj where
    pickleRef _ = xpickle

instance RefPickler ObjRef where
    pickleRef kind =
        case kind of
          UseRef -> xmlAttr "use" readShowIso
          DefRef -> xmlAttr "ref" readShowIso

$(defineIsomorphisms ''ObjLink)

xpObjLink :: (HasObjType a, RefPickler r, XmlPickler (a r), Show (a r), XmlSyntax d) => d (ObjLink r a)
xpObjLink = objLink <$> pickleRef DefRef

xpObjUse :: (HasObjType a, RefPickler r, XmlPickler (a r), Show (a r), XmlSyntax d) => d (ObjLink r a)
xpObjUse = objLink <$> pickleRef UseRef

-- ---------------------------------------------------------------------------
--  ObjRef lists
-- ---------------------------------------------------------------------------
xpObjRefList :: (HasObjType t, RefPickler r, XmlPickler (t r), Show' r, Show (t r), XmlSyntax d) =>
                TL.Text
             -> TL.Text
             -> Iso ([ObjLink (r :: * -> *) (t :: (* -> *) -> *)]) (t' r)
             -> d (t' r)
xpObjRefList lname elname iso =
    iso <$> elemCp lname (xmlFixedAttr "version" "1.0" *>
                          many (elemCp elname xpObjLink))


-- ---------------------------------------------------------------------------
--  Objects
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''WithObjId)
$(defineIsomorphisms ''ObjData)
$(defineIsomorphisms ''ObjDataMetaValue)
$(defineIsomorphisms ''WithObjType)

xpObjDataMeta :: XmlSyntax d => d ObjDataMeta
xpObjDataMeta =
    listMapIso <$>
    (many $ elemSync "meta" (xmlAttr "key" textStringIso <*>
                             (xmlFixedAttr "type" "xsd:string" *> (objMetaString <$> xmlString)
                              <|> xmlFixedAttr "type" "xsd:date" *> (dateIso <$> xmlString)
                              <|> xmlFixedAttr "type" "xsd:dateTime" *> (dateTimeIso <$> xmlString))))
    where
      dateIso :: Iso String ObjDataMetaValue
      dateIso = objMetaDate . unsafeMakeIso parseXsdDate (Just . formatXsdDate)
      dateTimeIso :: Iso String ObjDataMetaValue
      dateTimeIso = objMetaDateTime . unsafeMakeIso parseXsdDateTime (Just . formatXsdDateTime)


instance (HasObjType t, RefPickler r) => XmlPickler (WithObjId ObjData (t (r :: * -> *))) where
    xpickle =
        let iso = unsafeMakeIso unwrapWot (Just . wrapWot)
        in iso <$> xpDocument

instance RefPickler r => XmlPickler (WithObjType (WithObjId ObjData) r) where
    xpickle = xpDocument

xpDocument :: (XmlSyntax x, RefPickler r) =>
              x (WithObjType (WithObjId ObjData) r)
xpDocument =
        elemSync "document" (xmlFixedAttr "version" "1.0" *>
                             (isRoot <$> obj "root"
                              <|> isUserList <$> obj "user-list"
                              <|> isUser <$> obj "user"
                              <|> isWardList <$> obj "ward-list"
                              <|> isWard <$> obj "ward"
                              <|> isPatientList <$> obj "patient-list"
                              <|> isPatient <$> obj "patient"
                              <|> isPatientRecord <$> obj "patient-record"
                              <|> isIDocLink <$> obj "idoc-link"
                              <|> isIDoc <$> obj "idoc"
                              <|> isResource <$> obj "resource"
                              <|> isTabList <$> obj "tab-list"
                              <|> isTabContent <$> obj "tab-content"
                              <|> isTab <$> obj "tab"
                              <|> isCategory <$> obj "category"
                             ))
        where
          obj prefix = withObjId <$> xmlAttr "id" (readShowIso . mkIso prefix)
                                     <*> (objData <$> xpObjDataMeta <*> xpickle)
          mkIso :: TL.Text -> Iso TL.Text TL.Text
          mkIso prefix =
              let checkPrefix s = if TL.snoc prefix '_' `TL.isPrefixOf` s then Just s else Nothing
              in unsafeMakeIso checkPrefix checkPrefix

-- ---------------------------------------------------------------------------
--  Root
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''RootData)

instance (Show' r, RefPickler r) => XmlPickler (RootData r) where
    xpickle =
        rootData <$> elemCp "root" (xmlFixedAttr "version" "1.0" *>
                                    elemCp "user-list" xpObjLink <*>
                                    elemCp "tab-list" xpObjLink)

-- ---------------------------------------------------------------------------
--  TabList
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''TabListData)

instance (Show' r, RefPickler r) => XmlPickler (TabListData r) where
    xpickle = xpObjRefList "tab-list" "tab" tabListData


-- ---------------------------------------------------------------------------
--  Tab
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''TabData)

instance (Show' r, RefPickler r) => XmlPickler (TabData r) where
    xpickle =
        tabData <$> elemCp "tab" (xmlFixedAttr "version" "1.0" *>
                                  elemCp "name" xmlString <*>
                                  elemCp "icon" xpObjLink)

-- ---------------------------------------------------------------------------
--  UserList
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''UserListData)

instance (Show' r, RefPickler r) => XmlPickler (UserListData r) where
    xpickle =
        userListData <$> elemCp "user-list" (xmlFixedAttr "version" "1.0" *>
                                             many (elemCp "user" xpObjLink))

-- ---------------------------------------------------------------------------
--  User
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''UserData)

$(defineIsomorphisms ''DocTypes.Name)

instance (Show' r, RefPickler r) => XmlPickler (UserData r) where
    xpickle =
        userData <$> elemCp "user" (xmlFixedAttr "version" "1.0" *>
                                    elemCp "name" xpName <*>
                                    elemCp "login" xmlString <*>
                                    elemCp "password" xmlString <*>
                                    elemCp "ward-list" xpObjLink)

xpName :: XmlSyntax d => d DocTypes.Name
xpName =
    name <$> optional (elemCp "title" xmlString) <*>
             many (elemCp "given-name" xmlString) <*>
             elemCp "family-name" xmlString <*>
             elemCp "display-name" xmlString

-- ---------------------------------------------------------------------------
--  WardList
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''WardListData)

instance (Show' r, RefPickler r) => XmlPickler (WardListData r) where
    xpickle =
        wardListData <$> elemCp "ward-list" (xmlFixedAttr "version" "1.0" *>
                                             many (elemCp "ward" xpObjLink))

-- ---------------------------------------------------------------------------
--  Ward
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''WardData)

instance (Show' r, RefPickler r) => XmlPickler (WardData r) where
    xpickle =
        wardData <$> elemCp "ward" (xmlFixedAttr "version" "1.0" *>
                                    elemCp "name" xmlString <*>
                                    optional (elemCp "idoc" xpObjLink) <*>
                                    elemCp "patient-list" xpObjLink)

-- ---------------------------------------------------------------------------
--  PatientList
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''PatientListData)

instance (Show' r, RefPickler r) => XmlPickler (PatientListData r) where
    xpickle = xpObjRefList "patient-list" "patient" patientListData

-- ---------------------------------------------------------------------------
--  Patient
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''PatientData)

$(defineIsomorphisms ''PatientInfo)

instance (Show' r, RefPickler r) => XmlPickler (PatientData r) where
    xpickle =
        patientData <$> elemCp "patient" (xmlFixedAttr "version" "1.0" *>
                                          xpPatInfo <*>
                                          elemCp "patient-record" xpObjLink)

xpPatInfo :: XmlSyntax d => d PatientInfo
xpPatInfo =
    patientInfo <$>
    elemCp "header" (many (elemCp "line" xmlString)) <*>
    elemCp "description" (many (elemCp "line" xmlString))

-- ---------------------------------------------------------------------------
--  PatientRecord
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''PatientRecordData)

instance (Show' r, RefPickler r) => XmlPickler (PatientRecordData r) where
    xpickle =
        patientRecordData <$>
        elemCp "patient-record" (xmlFixedAttr "version" "1.0" *>
                                 xpPatInfo <*>
                                 elemCp "tab-content-list" (many (elemCp "tab-content" xpObjLink)))

-- ---------------------------------------------------------------------------
--  TabContent
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''TabContentData)

instance (Show' r, RefPickler r) => XmlPickler (TabContentData r) where
    xpickle =
        tabContentData <$> elemCp "tab-content" (xmlFixedAttr "version" "1.0" *>
                                                 elemCp "tab" xpObjUse <*>
                                                 elemCp "category-list"
                                                        (many (elemCp "category" xpObjLink)))

-- ---------------------------------------------------------------------------
--  Category
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''CategoryData)

instance (Show' r, RefPickler r) => XmlPickler (CategoryData r) where
    xpickle =
        categoryData <$> elemCp "category" (xmlFixedAttr "version" "1.0" *>
                                            elemCp "title" xmlString <*>
                                            elemCp "idoc" xpObjLink <*>
                                            elemCp "content-list" (    left <$> many (elemCp "idoc" xpObjLink)
                                                                   <|> right <$> many (elemCp "ilink" xpObjLink)))

-- ---------------------------------------------------------------------------
--  IDocLink
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''IDocLinkData)

instance (Show' r, RefPickler r) => XmlPickler (IDocLinkData r) where
    xpickle = xpIDocLink

xpIDocLink :: (Show' r, RefPickler r, XmlSyntax d) => d (IDocLinkData r)
xpIDocLink =
    iDocLinkData <$> elemIDoc "idoc-link" (xmlFixedAttr "version" "1.0" *>
                                           elemIDoc "tuple" xmlString <*>
                                           optional (elemIDoc "description" xmlString) <*>
                                           optional (elemIDoc "icon" xpObjLink) <*>
                                           elemIDoc "idoc" xpObjLink)

-- ---------------------------------------------------------------------------
--  IDoc
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''IDocData)
$(defineIsomorphisms ''IDocMeta)
$(defineIsomorphisms ''IDocBody)
$(defineIsomorphisms ''IDocCell)
$(defineIsomorphisms ''IDocSection)
$(defineIsomorphisms ''IDocPage)
$(defineIsomorphisms ''IDocResource)

instance (Show' r, RefPickler r) => XmlPickler (IDocData r) where
    xpickle =
        iDocData <$> elemIDoc "idoc" (xmlFixedAttr "version" "1.3" *>
                                      xpIDocMeta <*>
                                      xpIDocBody)

xpIDocMeta :: (Show' r, RefPickler r, XmlSyntax d) => d (IDocMeta r)
xpIDocMeta = iDocMeta <$>
    elemIDoc "meta" (elemIDoc "title" xmlString <*>
                     optional (elemIDoc "description" xmlString) <*>
                     optional (elemIDoc "icon" xpObjLink) <*>
                     optional (elemIDoc "thumbnail" xpObjLink))

instance (RefPickler r, Show' r) => XmlPickler (IDocBody r) where
    xpickle = xpIDocBody

xpIDocBody :: (Show' r, RefPickler r, XmlSyntax d) => d (IDocBody r)
xpIDocBody =
    iDocBodyPage <$> elemIDoc "page"
                     (iDocPage <$> many (iDocSection <$>
                                         (elemIDoc "section" (optional (xmlAttr "title" textStringIso) <*>
                                                                       many xpIDocCell))))
    <|> iDocBodyResource <$>
            (iDocSingleResource <$> elemIDoc "resource" xpObjLink
             <|> iDocResourceGroup <$> elemIDoc "resource-group" (many (elemIDoc "resource" xpObjLink)))

xpIDocCell :: (Show' r, RefPickler r, XmlSyntax d) => d (IDocCell r)
xpIDocCell =
    (iDocKeyValueCell <$> elemIDoc "key-value" (xmlAttr "key" textStringIso <*> xmlString)
     <|> iDocParagraphCell <$> elemIDoc "paragraph" xmlString
     <|> iDocResourceLink <$> (left <$> elemIDoc "resource-link" xpObjLink <|> right <$> xpIDocLink))

-- ---------------------------------------------------------------------------
--  Resource
-- ---------------------------------------------------------------------------

$(defineIsomorphisms ''ResourceData)

instance (Show' r, RefPickler r) => XmlPickler (ResourceData r) where
    xpickle =
        resourceData <$>
                     elemCp "content-type" xmlString <*>
                     elemCp "data" (base64 <$> xmlString)

base64 :: Iso String BSL.ByteString
base64 =
    let (f, g) = wrapBase64
    in unsafeMakeIso f (Just . g)

baseToStr :: ([Word8] -> String) -> BSL.ByteString -> String
baseToStr f = f . BSL.unpack

strToBase64 :: (String -> Maybe [Word8]) -> String -> Maybe BSL.ByteString
strToBase64 f = fmap BSL.pack . f

wrapBase64 :: (String -> Maybe BSL.ByteString, BSL.ByteString -> String)
wrapBase64 = (strToBase64 Base64.decode, baseToStr Base64.encode)

--
--
--

main :: IO ()
main =
    do args <- getArgs
       case args of
         [] -> fail "no commandline arguments given"
         ["--benchmark-parsing", fname] -> benchmarkParsing fname
         ["--benchmark-printing", fname, s] -> benchmarkPrinting fname (read s :: Int)
         ["--benchmark-printing-simple", fname, s] -> benchmarkPrintingSimple fname (read s :: Int)
         _ -> mapM_ doRoundtrip (filter (\s -> not ("resource" `isInfixOf` s)) args)

benchmarkParsing fname =
    do putStrLn $ "Parsing " ++ fname ++ " ... "
       x :: IDocBody ObjRef <- parseFromFile fname
       putStrLn $ "parsed " ++ show fname ++ ", length (show x) = " ++ show (length (show x))

benchmarkPrintingSimple fname n =
    do putStrLn "Printing ... "
       unparseToFile fname (many (xmlElem "foo" (pure ()))) (replicate n ())
       putStrLn "Done printing"

benchmarkPrinting fname n =
    do putStrLn "Printing ... "
       unparseToFile fname xpickle (IDocBodyPage page :: IDocBody ObjRef)
       putStrLn "Done printing"
    where
      page = {-# SCC "page" #-} IDocPage (map mkSection [1..n])
      mkSection i = IDocSection (Just (show i)) (map mkCell [1..100])
      mkCell _ = IDocKeyValueCell "key" "value"

doRoundtrip fname =
    do putStrLn $ "Doing roundtrip for " ++ fname ++ " ... "
       x <- parseFromFile fname
       putStrLn $ "parsed " ++ show fname
       bs <- unparse x
       putStrLn $ "Resulting after parse / unparse cycle: "
       BSL.putStrLn bs
       x' <- parseFromBytes bs
       if (x == x')
          then putStrLn "Everything OK!"
          else putStrLn ("Roundtrip FAILED, object after 1st parse:\n" ++ show x ++
                         "\nobject after 2nd parse:\n" ++ show x')

parseFromFile :: forall a . XmlPickler a => FilePath -> IO a
parseFromFile fname =
    do env <- getEnvironment
       case List.lookup "DEBUG" env of
         Just "1" ->
             do putStrLn "Using debugParse function"
                putStrLn "Specification:"
                let spec = runSpecPrinter (xpickle :: SpecPrinter a)
                putStrLn spec
                debugParse
         _ -> realParse
    where
      debugParse :: IO a
      debugParse =
          do events <- run_ $
                       joinI $
                       enumFile fname $$
                       parseBytes $$
                       consume
             case runXmlParser fname defaultEntityRenderer events xpickle of
               Left err -> fail (show err)
               Right x -> return x
      realParse :: IO a
      realParse =
          do run_ $ joinI $
                    enumFile fname $$
                    parseBytes $$
                    parseXml fname defaultEntityRenderer xpickle

parseFromBytes :: BSL.ByteString -> IO AnyRefObj
parseFromBytes bs =
    run_ $ joinI $
           enumList 1 (BSL.toChunks bs) $$
           parseBytes $$
           parseXml "<from bytes>" defaultEntityRenderer xpickle

unparse :: XmlPickler a => a -> IO BSL.ByteString
unparse x =
    do l <- run_ $
            xmlPrintEnumerator xpickle x $$
            joinI $ renderBytes $$
            consume
       return $ BSL.fromChunks l

unparseToFile :: FilePath -> XmlPrinter a -> a -> IO ()
unparseToFile fname xspec x =
    do h <- openFile fname WriteMode
       run_ $
         xmlPrintEnumerator xspec x $$
         joinI $ renderBytes $$
         iterHandle h
       hClose h
