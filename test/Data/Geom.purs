module Test.Data.Geom where

import Data.Geom
import Data.Geom.Monoid
import Data.Geom.QuickCheck
import Data.Geom.Show
import Test.QuickCheck
import Test.Properties

equalityWorks :: Pair -> Boolean
equalityWorks p = p == p

semigroupProperty :: Pair -> Pair -> Pair -> Result
semigroupProperty = verifySemigroup

monoidProperty :: Pair -> Result
monoidProperty = verifyMonoid

subtraction :: Pair -> Pair -> Result
subtraction a b =
  a == subPairs (a <> b) b <?>
  "Property: (a+b)-b == a" ++
  "\n     a = " ++ show a ++
  "\n     b = " ++ show b ++
  "\n   a+b = " ++ show (a <> b) ++
  "\nresult = " ++ show (subPairs (a <> b) b) ++ "\n"

tests = do
  quickCheck equalityWorks
  quickCheck semigroupProperty
  quickCheck monoidProperty
  quickCheck subtraction
