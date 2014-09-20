module Data.Geom.Transform.Monoid where

import Data.Geom.Transform
import Data.Monoid

instance monoidTransform :: Monoid Transform where
  mempty = reset
