module Data.Geom.QuickCheck where

import Data.Geom
import Test.QuickCheck
import qualified Test.QuickCheck.LCG as LCG

mkPair = do
  x <- LCG.uniform
  y <- LCG.uniform
  return $ Pair x y

instance arbPair :: Arbitrary Pair where
  arbitrary = mkPair
