module Data.Geom.QuickCheck where

import Data.Geom
import Test.QuickCheck
import Test.QuickCheck.LCG

mkPair = do
  x <- uniform
  y <- uniform
  return $ Pair x y

instance arbPair :: Arbitrary Pair where
  arbitrary = mkPair
