<html>
<head>
<title>Roundtrip Tutorial</title>
</head>
<body>

<h1>Roundtrip Tutorial</h1>

<i>DRAFT DRAFT DRAFT</i>

<h2>Testing and using the specifications</h2>

> import Spec
>
> import Data.Maybe (fromJust, isJust)
> import Control.Monad (liftM2)
>
> import Test.QuickCheck
>
> import Text.Roundtrip.Parser
> import Text.Roundtrip.Printer
>
> import Text.Roundtrip.Xml.Parser
> import Text.Roundtrip.Xml.Printer
>
> testParser :: Either ParseError Expr
> testParser = runStringParser pExpr "<string>" "(1+(foo+2))"
>
> testPrinter :: Maybe String
> testPrinter = runPrinter pExpr (Plus (Lit 1) (Plus (Var "foo") (Lit 2)))
>
> instance Arbitrary Expr where
>     arbitrary = sized arbExpr
>         where arbExpr 0 = frequency simpleExprs
>               arbExpr n = frequency (simpleExprs ++
>                                      [(5, liftM2 Plus (arbExpr (n `div` 2))
>                                                       (arbExpr (n `div` 2)))])
>               simpleExprs = [(1, do n <- arbitrary
>                                     return (Lit n)),
>                              (1, do v <- elements letters
>                                     vs <- listOf (elements lettersOrDigits)
>                                     return (Var (v:vs)))]
>               letters = ['a'..'z'] ++ ['A'..'Z']
>               lettersOrDigits = letters ++ ['0'..'9']
>     shrink (Var _) = []
>     shrink (Lit _) = []
>     shrink (Plus e1 e2) = [e1, e2]
>
> prop_printerDoesNotFail :: Expr -> Bool
> prop_printerDoesNotFail expr = isJust (runStringPrinter pExpr expr)
>
> prop_printerParserInverse :: Expr -> Bool
> prop_printerParserInverse expr =
>     let code = fromJust (runStringPrinter pExpr expr)
>     in case runStringParser pExpr "<string>" code of
>          Left err -> error (show err)
>          Right expr' -> expr == expr'
>
> testXmlParser :: Either ParseError Expr
> testXmlParser = runXmlParserString pXmlExpr "<string>" defaultEntityRenderer
>                   "<plus><lit value=\"1\"/><plus><var name=\"foo\"/><lit value=\"2\"/></plus></plus>"
>
> testXmlPrinter :: Maybe String
> testXmlPrinter = runXmlPrinterString pXmlExpr (Plus (Lit 1) (Plus (Var "foo") (Lit 2)))
>
> prop_xmlPrinterDoesNotFail :: Expr -> Bool
> prop_xmlPrinterDoesNotFail expr = isJust (runXmlPrinterString pXmlExpr expr)
>
> prop_xmlPrinterParserInverse :: Expr -> Bool
> prop_xmlPrinterParserInverse expr =
>     let code = fromJust (runXmlPrinterString pXmlExpr expr)
>     in case runXmlParserString pXmlExpr "<string>" defaultEntityRenderer code of
>          Left err -> error (show err)
>          Right expr' -> expr == expr'
>
> allProperties = [prop_printerDoesNotFail, prop_printerParserInverse,
>                  prop_xmlPrinterDoesNotFail, prop_xmlPrinterParserInverse]
>
> qc = mapM_ quickCheck allProperties

</body>
</html>
