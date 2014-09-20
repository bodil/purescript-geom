module Test.Main where

import Test.QuickCheck

main = do
  Test.Data.Geom.tests
  Test.Data.Geom.Transform.tests
