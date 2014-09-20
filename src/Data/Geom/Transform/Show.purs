module Data.Geom.Transform.Show where

import Data.Geom.Transform

instance showTransform :: Show Transform where
  show (Transform a b c d e f g h i) =
    "Transform(" ++ round a ++ " " ++ round b ++ " " ++ round c ++
    "  " ++ round d ++ " " ++ round e ++ " " ++ round f ++
    "  " ++ round g ++ " " ++ round h ++ " " ++ round i ++ ")"
    where round a = show $ ((a * 10000) | 0) / 10000
