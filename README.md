# Module Documentation

A library for doing matrix transformations on 2D cartesian coordinates.

```purescript
λ transformPoint (translate 3 4) (Pair 1 2)
(4, 6)
```

## Module Data.Geom

### Types

    data Pair where
      Pair :: Number -> Number -> Pair

    type Point  = Pair

    type Size  = Pair


### Type Class Instances

    instance eqPair :: Eq Pair

    instance semigroupPair :: Semigroup Pair


### Values

    addPairs :: Pair -> Pair -> Pair

    subPairs :: Pair -> Pair -> Pair


## Module Data.Geom.Monoid

### Type Class Instances

    instance monoidPair :: Monoid Pair


## Module Data.Geom.QuickCheck

### Type Class Instances

    instance arbPair :: Arbitrary Pair


## Module Data.Geom.Show

### Type Class Instances

    instance showPair :: Show Pair


## Module Data.Geom.Transform

### Types

    data Transform where
      Transform :: Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Transform


### Type Class Instances

    instance eqTransform :: Eq Transform

    instance semigroupTransform :: Semigroup Transform


### Values

    compose :: Transform -> Transform -> Transform

    determinant :: Transform -> Number

    inverse :: Transform -> Transform

    mulN :: Number -> Transform -> Transform

    reset :: Transform

    rotate :: Number -> Transform

    scale :: Number -> Transform

    scaleX :: Number -> Transform

    scaleY :: Number -> Transform

    transformPoint :: Transform -> Point -> Point

    translate :: Number -> Number -> Transform


## Module Data.Geom.Transform.Monoid

### Type Class Instances

    instance monoidTransform :: Monoid Transform


## Module Data.Geom.Transform.QuickCheck

### Type Class Instances

    instance arbTransform :: Arbitrary Transform


## Module Data.Geom.Transform.Show

### Type Class Instances

    instance showTransform :: Show Transform
