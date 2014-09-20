module Data.Geom.Show where

import Data.Geom

instance showPair :: Show Pair where
  show (Pair x y) = "(" ++ round x ++ ", " ++ round y ++ ")"
    where round a = show $ ((a * 10000) | 0) / 10000
