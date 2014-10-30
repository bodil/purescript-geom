module Data.Geom where

import Math

data Pair = Pair Number Number

instance eqPair :: Eq Pair where
  (==) (Pair x1 y1) (Pair x2 y2) =
    let sortOf a b = round (a * 10000) == round (b * 10000)
    in sortOf x1 x2 && sortOf y1 y2
  (/=) a b = not (a == b)

addPairs :: Pair -> Pair -> Pair
addPairs (Pair x1 y1) (Pair x2 y2) = Pair (x1+x2) (y1+y2)

subPairs :: Pair -> Pair -> Pair
subPairs (Pair x1 y1) (Pair x2 y2) = Pair (x1-x2) (y1-y2)

type Point = Pair
type Size = Pair

instance semigroupPair :: Semigroup Pair where
  (<>) = addPairs
