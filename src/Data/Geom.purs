module Data.Geom where

import Data.Monoid
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

instance monoidPair :: Monoid Pair where
  mempty = Pair 0 0

instance showPair :: Show Pair where
  show (Pair x y) = "(" ++ round x ++ ", " ++ round y ++ ")"
    where round a = show $ ((a * 10000) .|. 0) / 10000
