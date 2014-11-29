module Data.Geom.Transform.QuickCheck where

import Data.Geom.Transform
import Test.QuickCheck
import Test.QuickCheck.Gen

mkTransform :: Gen Transform
mkTransform = do
  a <- uniform
  b <- uniform
  c <- uniform
  d <- uniform
  e <- uniform
  f <- uniform
  g <- uniform
  h <- uniform
  i <- uniform
  return $ Transform a b c d e f g h i

instance arbTransform :: Arbitrary Transform where
  arbitrary = mkTransform
