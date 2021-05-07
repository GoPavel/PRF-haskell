{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, FunctionalDependencies #-}
{-
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, TypeOperators, InstanceSigs, FlexibleContexts #-}
-}

module Main where

z :: Int -> Int
z _ = 0

inc :: Int -> Int
inc x = x + 1

class Prj1 a r where
  prj1 :: a -> r

-- a -> a
instance Prj1 a a where
  prj1 x = x

-- a -> r ==> a -> b -> r ==> a -> b -> c -> r ==> ...
instance (Prj1 a r) => Prj1 a (b -> r) where
  prj1 = \a _b -> prj1 a

-- Example
ex1 :: Int
ex1 = prj1 (2 :: Int) "" "" (42.0 :: Double)

class Prj2 r where
  prj2 :: Int -> Int -> r

-- a -> b -> b
instance Prj2 Int where
  prj2 x y = y

-- a -> b -> r ==> a -> b -> a -> r
instance (Prj2 r) => Prj2 (Int -> r) where
  prj2 = \a b _c -> prj2 a b

-- Example
-- ex2 :: String
-- ex2 = prj2 (2 :: Int) "" "" (42.0 :: Double)

class Prj3 c r where
  prj3 :: a -> b -> c -> r
instance Prj3 c c where
  prj3 _ _ c = c
instance (Prj3 c r) => Prj3 c (b -> r) where
  prj3 a b c _ = prj3 a b c

class Prj4 d r where
  prj4 :: a -> b -> c -> d -> r
instance Prj4 d d where
  prj4 _ _ _ d = d
instance (Prj4 d r) => Prj4 d (b -> r) where
  prj4 a b c d _ = prj4 a b c d

class R r where
  r :: (x -> r) -> (Int -> Int -> x -> r) -> Int -> x -> r

-- (x -> c) -> (y -> c -> x -> c) -> y -> x -> c
instance R Int where
  r f g y x | y == 0     = f x
            | otherwise = g (y-1) (r f g (y-1) x) x

-- (x1 -> c) -> (y -> c -> x1 -> c) -> y -> x1 -> c
-- ===>
-- (x1 -> x2 -> c) -> (y -> c -> x1 -> x2 -> c) -> y -> x1 -> x2 -> c
instance (R r) => R (x -> r) where
  r f g y x1 x2 = r f' g' y (x1, x2)
    where
      f' (x1, x2) = f x1 x2
      g' y res (x1, x2) = g y res x1 x2


-- Example: dec
ex3 :: Int
ex3 = r z prj1 5 (5::Int)

-- U f g_j x_i = f (g_j (x_i)
-- :: (b -> c) -> (x -> b) -> x -> c
-- :: (b1 -> b2 -> c) -> (x1 -> b1) -> (x1 -> b2) -> x1 -> c
-- :: (b -> c) -> (x1 -> x2 -> b) -> x1 -> x2 -> c

class U b c r1 r2 where
  c :: (b -> c) -> (x -> r1) -> x -> r2

instance U b c b c where
  c f g x = f $ g x

instance (U b c r1 r2) => U b c (x -> r1) (x -> r2) where
  c f g = curry (c f (uncurry g))

class U2 b1 b2 c r1 r2 r3 where
  c2 :: (b1 -> b2 -> c) -> (x1 -> r1) -> (x1 -> r2) -> x1 -> r3
instance U2 b1 b2 c b1 b2 c where
  c2 f g1 g2 x1 = f (g1 x1) (g2 x1)
instance (U2 b1 b2 c r1 r2 r3) => U2 b1 b2 c (x2 -> r1) (x2 -> r2) (x2 -> r3) where
  c2 f g1 g2 = curry (c2 f (uncurry g1) (uncurry g2))

-- Example dec

idInt :: Int -> Int
idInt = prj1

ex4 :: Int -> Int
ex4 = c2 (r z prj1) idInt idInt

-- add
-- R f g y x | y == 0 f (x)
--           | y >< 0 g (y, R, x) -> U inc prj2
add :: Int -> Int -> Int
add = r prj1 (c inc (prj2 :: Int -> Int -> Int -> Int))

-- mul
mul :: Int -> Int -> Int
mul = r z mulg
  where
    prj2', prj3' :: Int -> Int -> Int -> Int
    prj2' = prj2
    prj3' = prj3
    mulg = c2 add prj2' prj3'

main = print "Hello"
