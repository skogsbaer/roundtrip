module Control.Isomorphism.Partial.Prim
  ( idIso
  , inverse
  , apply
  , unapply
  , IsoFunctor ((<$>))
  , ignore
  , (***)
  , (|||)
  , associate
  , commute
  , unit
  , element
  , subset
  , namedSubset
  , iterateIso
  , distribute
  , readShowIso
  , readShowTextIso
  , textStringIso
  , lazyStrictTextIso
  , listMapIso
  , maybeUnitBoolIso
  ) where

import Prelude hiding ((.), id)

import Control.Monad (liftM2, (>=>), fmap, mplus)
import Control.Category (Category (id, (.)))

import Data.Bool (Bool, otherwise)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Maybe (Maybe (Just, Nothing))
import qualified Data.Map as Map

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Safe (readMay)

import Control.Isomorphism.Partial.Iso

inverse :: Iso alpha beta -> Iso beta alpha
inverse iso = unsafeMakeIso' name' (isoShowSR iso) (isoShowSL iso) (isoRL iso) (isoLR iso)
    where
      name' = "inverse(" ++ isoName iso ++ ")"

apply :: Iso alpha beta -> alpha -> Maybe beta
apply = isoLR

unapply  ::  Iso alpha beta -> beta -> Maybe alpha
unapply = isoRL

idIso :: Iso a a
idIso = unsafeMakeNamedIso "id" Just Just

instance Category Iso where
  g . f = unsafeMakeIso' name' (isoShowSL f) (isoShowSR g)
                        (apply f >=> apply g) (unapply g >=> unapply f)
      where
        name' = "(" ++ isoName g ++ " . " ++ isoName f ++ ")"
  id = idIso

infix 5 <$>

class IsoFunctor f where
  (<$>) :: Iso alpha beta -> (f alpha -> f beta)

ignore :: alpha -> Iso alpha ()
ignore x = unsafeMakeNamedIsoR "ignore" f g where
  f _   =  Just ()
  g ()  =  Just x

-- | the product type constructor `(,)` is a bifunctor from
-- `Iso` $\times$ `Iso` to `Iso`, so that we have the
-- bifunctorial map `***` which allows two separate isomorphisms
-- to work on the two components of a tuple.
(***) :: Iso alpha beta -> Iso gamma delta -> Iso (alpha, gamma) (beta, delta)
i *** j = unsafeMakeIso' name (showPair isoShowSL isoShowSL) (showPair isoShowSR isoShowSR) f g
    where
      f (a, b) = liftM2 (,) (apply i a) (apply j b)
      g (c, d) = liftM2 (,) (unapply i c) (unapply j d)
      name = "(" ++ isoName i ++ " *** " ++ isoName j ++ ")"
      showPair f g =
          case (f i, g j) of
            (Just si, Just sj) -> Just $ \(x,y) -> showChar '(' . si x . showString ", "
                                                   . sj y . showString ")"
            _ -> Nothing

-- | The mediating arrow for sums constructed with `Either`.
-- This is not a proper partial isomorphism because of `mplus`.
(|||) :: Iso alpha gamma -> Iso beta gamma -> Iso (Either alpha beta) gamma
i ||| j = unsafeMakeIso' name showEither (isoShowSR i `mplus` isoShowSR j) f g
    where
      f (Left x) = apply i x
      f (Right x) = apply j x
      g y = (Left `fmap` unapply i y) `mplus` (Right `fmap` unapply j y)
      name = "(" ++ isoName i ++ " ||| " ++ isoName j ++ ")"
      showEither =
          case (isoShowSL i, isoShowSL j) of
            (Just si, Just sj) -> Just $ \e -> case e of
                                                 Left x -> showChar '(' . showString "Left " .
                                                           si x . showChar ')'
                                                 Right x -> showChar '(' . showString "Right " .
                                                            sj x . showChar ')'
            _ -> Nothing

-- | Nested products associate.
associate :: Iso (alpha, (beta, gamma)) ((alpha, beta), gamma)
associate = unsafeMakeIso f g where
  f (a, (b, c)) = Just ((a, b), c)
  g ((a, b), c) = Just (a, (b, c))

-- | Products commute.
commute :: Iso (alpha, beta) (beta, alpha)
commute = unsafeMakeIso f f where
  f (a, b) = Just (b, a)

-- | `()` is the unit element for products.
unit :: Iso alpha (alpha, ())
unit = unsafeMakeNamedIso "unit" f g
    where
      f a = Just (a, ())
      g (a, ()) = Just a

-- | Products distribute over sums.
distribute :: Iso (alpha, Either beta gamma) (Either (alpha, beta) (alpha, gamma))
distribute = unsafeMakeIso f g where
  f (a, Left   b)    =  Just (Left   (a, b))
  f (a, Right  c)    =  Just (Right  (a, c))
  g (Left   (a, b))  =  Just (a,  Left   b)
  g (Right  (a, b))  =  Just (a,  Right  b)

-- | `element x` is the partial isomorphism between `()` and the
-- singleton set which contains just `x`.
element :: (Show alpha, Eq alpha) => alpha -> Iso () alpha
element x = unsafeMakeNamedIsoR ("element(" ++ show x ++ ")")
  (\() -> Just x)
  (\b -> if x == b then Just () else Nothing)

-- | For a predicate `p`, `subset p` is the identity isomorphism
-- restricted to elements matching the predicate.
subset :: Show alpha => (alpha -> Bool) -> Iso alpha alpha
subset = namedSubset "?"

namedSubset :: Show alpha => String -> (alpha -> Bool) -> Iso alpha alpha
namedSubset name p = unsafeMakeNamedIsoLR ("subset(" ++ name ++ ")") f f where
  f x | p x = Just x
      | otherwise = Nothing

iterateIso :: Iso alpha alpha -> Iso alpha alpha
iterateIso step = unsafeMakeIso f g where
  f = Just . driver (apply step)
  g = Just . driver (unapply step)
  driver :: (alpha -> Maybe alpha) -> (alpha -> alpha)
  driver step state
    =  case step state of
         Just state'  ->  driver step state'
         Nothing      ->  state

readShowIso :: (Read a, Show a) => Iso String a
readShowIso = unsafeMakeNamedIsoLR "readShow" readMay (Just . show)

readShowTextIso :: (Read a, Show a) => Iso T.Text a
readShowTextIso = unsafeMakeNamedIsoLR "readShowText" (readMay . T.unpack) (Just . T.pack . show)

textStringIso :: Iso T.Text String
textStringIso = unsafeMakeNamedIsoLR "textString" (Just . T.unpack) (Just . T.pack)

lazyStrictTextIso :: Iso TL.Text T.Text
lazyStrictTextIso = unsafeMakeNamedIsoLR "lazyStrictText" lazyToStrict strictToLazy
    where
      lazyToStrict = Just . T.concat . TL.toChunks
      strictToLazy = Just . TL.fromChunks . (:[])

listMapIso :: Ord a => Iso ([(a, b)]) (Map.Map a b)
listMapIso = unsafeMakeNamedIso "listMap" (Just . Map.fromList) (Just . Map.toList)

maybeUnitBoolIso :: Iso (Maybe ()) Bool
maybeUnitBoolIso = unsafeMakeNamedIso "maybeUnitBoolIso" f g
    where f (Just ()) = Just True
          f _ = Just False
          g True = Just (Just ())
          g _ = Just Nothing
