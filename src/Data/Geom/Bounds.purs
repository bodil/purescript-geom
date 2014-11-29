module Data.Geom.Bounds where

import Data.Geom

-- |Bounds are represented by three points A, B and D such that:
--     A---B
--     |   |
--     D---C
-- Given that bounds are always rectangles, the missing point C can be inferred.
data Bounds = Bounds
              { ax :: Number, ay :: Number, bx :: Number, by :: Number,
                dx :: Number, dy :: Number }

boundsFor :: Point -> Point -> Point -> Bounds
boundsFor (Pair ax ay) (Pair bx by) (Pair dx dy) =
  Bounds { ax: ax, ay: ay, bx: bx, by: by, dx: dx, dy: dy }

axisAligned :: Bounds -> Boolean
axisAligned (Bounds b) = b.ax == b.dx && b.ay == b.by

pointInBounds :: Bounds -> Point -> Boolean
pointInBounds bounds@(Bounds b) (Pair x y) =
  if axisAligned bounds then
    b.ax <= x && b.ay <= y && x <= b.bx && y <= b.dy
  else let bax = b.bx - b.ax
           bay = b.by - b.ay
           dax = b.dx - b.ax
           day = b.dy - b.ay
           ab = (x - b.ax) * bax + (y - b.ay) * bay
           bb = (x - b.bx) * bax + (y - b.by) * bay
           ad = (x - b.ax) * dax + (y - b.ay) * day
           dd = (x - b.dx) * dax + (y - b.dy) * day
       in not $ (ab < 0) || (bb > 0) || (ad < 0) || (dd > 0)
