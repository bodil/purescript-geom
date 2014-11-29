module Data.Geom.QuickCheck where

import Data.Geom(Pair(..))
import Test.QuickCheck
import Test.QuickCheck.Gen

mkPair :: Gen Pair
mkPair = do
  x <- uniform
  y <- uniform
  return $ Pair x y

instance arbPair :: Arbitrary Pair where
  arbitrary = mkPair
