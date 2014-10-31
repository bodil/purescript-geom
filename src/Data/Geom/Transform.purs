module Data.Geom.Transform where

import Data.Geom
import Math

data Transform = Transform Number Number Number Number Number Number Number Number Number

-- This is ghastly inefficient, but all the rounding is required because
-- IEEE 754 introduces rounding errors. Thank you so much IEEE 754.
-- It is recommended not to compare transforms too frequently.
instance eqTransform :: Eq Transform where
  (==) (Transform a1 b1 c1 d1 e1 f1 g1 h1 i1) (Transform a2 b2 c2 d2 e2 f2 g2 h2 i2) =
    let sortOf a b = round (a * 10000) == round (b * 10000)
    in sortOf a1 a2 && sortOf b1 b2 && sortOf c1 c2 &&
       sortOf d1 d2 && sortOf e1 e2 && sortOf f1 f2 &&
       sortOf g1 g2 && sortOf h1 h2 && sortOf i1 i2
  (/=) a b = not (a == b)

-- |The identity transform is a transform such that
-- for any transform A and the identity transform I, A*I = A.
reset :: Transform
reset = Transform 1 0 0 0 1 0 0 0 1

-- |Create a transform that translates by a given vector.
translate :: Number -> Number -> Transform
translate x y = Transform 1 0 x 0 1 y 0 0 1

-- |Create a transform that rotates by a given angle.
rotate :: Number -> Transform
rotate angle = let c = cos angle
                   s = sin angle
               in Transform c (0-s) 0 s c 0 0 0 1

-- |Create a transform that rotates by a given angle around a given point.
rotateAround :: Number -> Number -> Number -> Transform
rotateAround x y angle =
  let r00 = cos angle
      r10 = sin angle
      r01 = (0 - r10)
  in Transform r00 r01 (x - (r00*x) - (r01*y)) r10 r00 (y - (r10*x) - (r00*y)) 0 0 1

-- |Create a transform that scales by a given factor.
scale :: Number -> Transform
scale s = Transform s 0 0 0 s 0 0 0 1

-- |Create a transform that scales only the X axis by a given factor.
scaleX :: Number -> Transform
scaleX s = Transform s 0 0 0 1 0 0 0 1

-- |Create a transform that scales only the Y axis by a given factor.
scaleY :: Number -> Transform
scaleY s = Transform 1 0 0 0 s 0 0 0 1

-- |Add two matrices together.
compose :: Transform -> Transform -> Transform
compose (Transform a1 b1 c1 p1 q1 r1 u1 v1 w1)
        (Transform a2 b2 c2 p2 q2 r2 u2 v2 w2) =
  Transform ((a1*a2) + (b1*p2) + (c1*u2))
            ((a1*b2) + (b1*q2) + (c1*v2))
            ((a1*c2) + (b1*r2) + (c1*w2))

            ((p1*a2) + (q1*p2) + (r1*u2))
            ((p1*b2) + (q1*q2) + (r1*v2))
            ((p1*c2) + (q1*r2) + (r1*w2))

            ((u1*a2) + (v1*p2) + (w1*u2))
            ((u1*b2) + (v1*q2) + (w1*v2))
            ((u1*c2) + (v1*r2) + (w1*w2))

-- |Calculate the determinant of a matrix.
determinant :: Transform -> Number
determinant (Transform a b c d e f g h i) =
  (a*(e*i-f*h)) - (b*(i*d - f*g)) + (c*(d*h - e*g))

-- |Multiply a matrix by a number.
mulN :: Number -> Transform -> Transform
mulN n (Transform a b c d e f g h i) =
  Transform (n*a) (n*b) (n*c) (n*d) (n*e) (n*f) (n*g) (n*h) (n*i)

-- |Calculate the inverse A⁻¹ of a transform A, such that A*A⁻¹ = I.
inverse :: Transform -> Transform
inverse t@(Transform a b c d e f g h i) =
  let det = 1 / determinant t
      t' = Transform (e*i - f*h) (0-((b*i) - (c*h))) (b*f - c*e)
                     (0-(d*i - f*g)) (a*i - c*g) (0-(a*f - c*d))
                     (d*h - e*g) (0-(a*h - b*g)) (a*e - b*d)
  in mulN det t'

-- |Apply a transform to a point.
transformPoint :: Transform -> Point -> Point
transformPoint (Transform a b c d e f g h i) (Pair x y) =
  Pair ((a*x) + (b*y) + c) ((d*x) + (e*y) + f)

-- Of course transforms form semigroups.
instance semigroupTransform :: Semigroup Transform where
  (<>) = compose
