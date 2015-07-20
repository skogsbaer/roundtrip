{-# LANGUAGE TemplateHaskell #-}
module Control.Isomorphism.Partial.TH
  ( defineIsomorphisms
  , defineIsomorphisms'
  ) where

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Language.Haskell.TH ( lamE, tupP, appE, conE, varP, caseE
                           , match, conP, normalB, newName, clause
                           , funD, mkName, reify, varE, nameBase
                           , Info(..), Dec(..), Con(..), wildP, tupE
                           , Name, Q, MatchQ
                           )
import Control.Monad (replicateM)
import Data.List (find)
import Data.Char (toLower)

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Isomorphism.Partial.Iso (Iso, unsafeMakeIso)

defineIsomorphisms :: Name -> Q [Dec]
defineIsomorphisms d = defineIsomorphisms' d (\(x:xs) -> (toLower x):xs)

defineIsomorphisms' :: Name -> (String -> String) -> Q [Dec]
defineIsomorphisms' d renameFun =
    do info <- reify d
       let cs = case info of
                  TyConI (DataD _ _ _ cs _) -> cs
                  TyConI (NewtypeD _ _ _ c _) -> [c]
                  otherwise -> error $ show d ++
                                       " neither denotes a data or newtype declaration. Found: " ++
                                       show info
       mapM (defFromCon (length cs > 1) renameFun) cs

defFromCon :: Bool -> (String -> String) -> Con -> Q Dec
defFromCon wc renameFun con@(NormalC n fields) = funCreation wc n (length fields) renameFun
defFromCon wc renameFun con@(RecC n fields) = funCreation wc n (length fields) renameFun
defFromCon wc renameFun con@(InfixC _ n _) = funCreation wc n 2 renameFun
defFromCon wc renameFun con@(ForallC _ _ _) = error $ "defineIsomorphisms not available for " ++
                                                      "existential data constructors"

funCreation :: Bool -> Name -> Int -> (String -> String) -> Q Dec
funCreation wc n nfields renameFun =
    funD (mkName $ renameFun $ nameBase n)
      [clause [] (normalB (isoFromCon (wildcard wc) n nfields)) []]

isoFromCon wildcard conName nfields =
    do (paths, exprs)  <-  genPE nfields
       dat <-  newName "x"
       let f = lamE [nested tupP paths]
                    [| Just $(foldl appE (conE conName) exprs) |]
       let g = lamE [varP dat]
                  (caseE (varE dat) $
                    [ match (conP conName paths)
                        (normalB [| Just $(nested tupE exprs) |]) []
                    ] ++ wildcard)
       [| unsafeMakeIso $f $g |]

wildcard :: Bool -> [MatchQ]
wildcard True = [match (wildP) (normalB [| Nothing |]) []]
wildcard _ = []

genPE number = do
  ids <- replicateM number (newName "x")
  return (map varP ids, map varE ids)

checkInfix :: Con -> Bool
checkInfix (InfixC _ _ _) = False
checkInfix _ = True

nested tup []      =  tup []
nested tup [x]     =  x
nested tup (x:xs)  =  tup [x, nested tup xs]
