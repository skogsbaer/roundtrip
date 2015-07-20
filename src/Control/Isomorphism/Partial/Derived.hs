module Control.Isomorphism.Partial.Derived (

    foldl, swap23, fixedValue

) where

import Prelude (Show, Eq)
import Control.Category (Category (id, (.)))

import Control.Isomorphism.Partial.Iso (Iso)
import Control.Isomorphism.Partial.Prim
import Control.Isomorphism.Partial.Constructors (cons, nil)

foldl :: Iso (alpha, beta) alpha -> Iso (alpha, [beta]) alpha
foldl i = inverse unit
        . (id *** inverse nil)
        . iterateIso (step i) where

  step i = (i *** id)
         . associate
         . (id *** inverse cons)

swap23 :: Iso (a, (b, c)) (a, (c, b))
swap23 = id *** commute

fixedValue :: (Show a, Eq a) => a -> Iso a ()
fixedValue = inverse . element
