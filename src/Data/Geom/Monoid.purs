module Data.Geom.Monoid where

import Data.Geom
import Data.Monoid

instance monoidPair :: Monoid Pair where
  mempty = Pair 0 0
