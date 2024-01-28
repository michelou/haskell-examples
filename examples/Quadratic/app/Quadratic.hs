{-|
Description: Quadratic equation and its solution using user-defined data types.
-}
module Quadratic where

import Data.Complex (Complex, Complex((:+)))

type RootT = Complex Double
data Roots = Roots RootT RootT deriving Show

-- data <type constructor> = <data constructor> ...
data Quadratic = Quadratic {
    a :: Double,
    b :: Double, 
    c :: Double
} deriving Show

-- | Calculates roots of a polynomial and return set of roots.
roots :: Quadratic -> Roots

-- Trivial, all constants are zero, error roots are not defined.
roots (Quadratic 0 0 _) = error "Not a quadratic polynomial"

-- Is a polynomial of degree 1, x = -c / b
roots (Quadratic 0 b1 c1) =
    let root = ( (-c1) / b1 :+ 0)
    in Roots root root

-- b^2 - 4ac = 0
roots (Quadratic a2 b2 c2) =
    let discriminant = b2 * b2 - 4 * a2 * c2
    in rootsInternal (Quadratic a2 b2 c2) discriminant

rootsInternal :: Quadratic -> Double -> Roots
-- Discriminant is zero, roots are real
rootsInternal q d | d == 0 =
    let r = (-(b q) / 2.0 / (a q))
        root = r :+ 0
    in Roots root root

-- Discriminant is negative, roots are complex.
rootsInternal q d | d < 0 =
    Roots (realpart :+ complexpart) (realpart :+ (-complexpart))
    where plusd = -d
          twoa = 2.0 * (a q)
          complexpart = (sqrt plusd) / twoa
          realpart = - (b q) / twoa

-- Discriminant is positive, all roots are real.
rootsInternal q d =
    Roots (root1 :+ 0) (root2 :+ 0)
    where plusd = -d
          twoa = 2.0 * (a q)
          dpart = (sqrt plusd) / twoa
          prefix = - (b q) / twoa
          root1 = prefix + dpart
          root2 = prefix - dpart
