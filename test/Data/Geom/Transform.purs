module Test.Data.Geom.Transform where

import Data.Geom
import Data.Geom.QuickCheck
import Data.Geom.Show
import Data.Geom.Transform
import Data.Geom.Transform.Monoid
import Data.Geom.Transform.QuickCheck
import Data.Geom.Transform.Show
import Test.QuickCheck
import Test.QuickCheck.LCG
import Test.Properties

equalityWorks :: Transform -> Boolean
equalityWorks t = t == t

semigroupProperty :: Transform -> Transform -> Transform -> Result
semigroupProperty = verifySemigroup

monoidProperty :: Transform -> Result
monoidProperty = verifyMonoid

tPlusInverseTIsReset :: Transform -> Result
tPlusInverseTIsReset t =
  t <> inverse t == reset <?>
  "Property: a*a⁻¹ == i" ++
  "\n     a = " ++ show t ++
  "\ndet(a) = " ++ show (determinant t) ++
  "\n   a⁻¹ = " ++ show (inverse t) ++
  "\nresult = " ++ show (t <> inverse t) ++ "\n"

aPlusBPlusInverseBIsA :: Transform -> Transform -> Result
aPlusBPlusInverseBIsA t1 t2 =
  t1 == (t1 <> t2) <> inverse t2 <?>
  "Property: (ab)b⁻¹ == a" ++
  "\n     a = " ++ show t1 ++
  "\n     b = " ++ show t2 ++
  "\n    ab = " ++ show (t1 <> t2) ++
  "\ndet(b) = " ++ show (determinant t2) ++
  "\n   b⁻¹ = " ++ show (inverse t2) ++
  "\nresult = " ++ show ((t1 <> t2) <> inverse t2) ++ "\n"

transformPoints :: Point -> Transform -> Transform -> Result
transformPoints p a b =
  (transformPoint b p) <> (transformPoint a p) ==
  (transformPoint a p) <> (transformPoint b p) <?>
  "Property: ap+bp = bp+ap" ++
  "\n    a = " ++ show a ++
  "\n    b = " ++ show b ++
  "\n    p = " ++ show p ++
  "\n   ap = " ++ show (transformPoint a p) ++
  "\n   bp = " ++ show (transformPoint b p) ++
  "\nap+bp = " ++ show ((transformPoint b p) <> (transformPoint a p)) ++
  "\nbp+ap = " ++ show ((transformPoint a p) <> (transformPoint b p)) ++ "\n"

tests = do
  quickCheck equalityWorks
  quickCheck semigroupProperty
  quickCheck monoidProperty
  quickCheck tPlusInverseTIsReset
  quickCheck aPlusBPlusInverseBIsA
  quickCheck transformPoints
