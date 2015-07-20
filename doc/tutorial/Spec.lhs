<html>
<head>
<title>Roundtrip Tutorial</title>
</head>
<body>

<h1>Roundtrip Tutorial</h1>

<i>DRAFT DRAFT DRAFT</i>

<p>
Roundtrip allows the definition of bidirectional (de-)serialization
specifications. The specification language is based on the ideas described
in the paper <i>Invertible Syntax Descriptions: Unifying Parsing and Pretty
Printing</i> by Tillmann Rendel and Klaus Ostermann, Haskell Symposium 2010
(<a href="http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf">online version</a>).
</p>

<p>
Author: Stefan Wehr (<code>wehr AT factisresearch DOT com</code>)<br>
Roundtrip on hackage:
<ul>
  <li><a href="http://hackage.haskell.org/package/roundtrip">roundtrip</a></li>
  <li><a href="http://hackage.haskell.org/package/roundtrip-string">roundtrip-string</a></li>
  <li><a href="http://hackage.haskell.org/package/roundtrip-xml">roundtrip-xml</a></li>
</ul>
</p>

<h2>Specification for parsing/printing expressions from strings</h2>

> {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
> module Spec where
>
> import Data.Char (isLetter, isDigit)
> import Text.Roundtrip
>
> data Expr = Var String
>           | Lit Int
>           | Plus Expr Expr
>             deriving (Show, Eq)
>
> $(defineIsomorphisms ''Expr)
>
> letter :: StringSyntax d => d Char
> letter =  token isLetter
>
> variable :: StringSyntax d => d String
> variable = cons <$> letter <*> many (letter <|> digit)
>
> digit :: StringSyntax d => d Char
> digit = token isDigit
>
> integer :: StringSyntax d => d Int
> integer = signedIntIso <$> optionalBool (char '-') <*> (readShowIso <$> many1 digit)
>     where
>       signedIntIso :: Iso (Bool, Int) Int
>       signedIntIso = let f (True, i) = Just (-i)
>                          f (False, i) = Just i
>                          g i = Just (i < 0, abs i)
>                      in unsafeMakeIso f g
>
> pExpr :: StringSyntax d => d Expr
> pExpr = var  <$> variable
>     <|> lit  <$> integer
>     <|> plus <$> (char '(' *> pExpr <*>
>                   char '+' *> pExpr <* char ')')

<h2>Specification for parsing/printing expressions from an XML tree</h2>

> {-
> <plus>
>   <var name="foo"/>
>   <lit value="123"/>
> </plus>
> -}
>
> xmlVariable :: XmlSyntax d => d String
> xmlVariable = xmlElem "var" (xmlAttr "name" textStringIso)
>
> xmlInteger :: XmlSyntax d => d Int
> xmlInteger = xmlElem "lit" (xmlAttr "value" readShowTextIso)
>
> pXmlExpr :: XmlSyntax d => d Expr
> pXmlExpr = var  <$> xmlVariable
>        <|> lit  <$> xmlInteger
>        <|> plus <$> xmlElem "plus" (pXmlExpr <*> pXmlExpr)

The <code><a href="Impl.html">Impl<a/></code> module shows these specifications in action.

</body>
</html>
