{-|
Module          : Sci.Calculus
Description     : A module for numerical calculus.
Maintainer      : nhalford@uchicago.edu
-}
module Sci.Calculus ( differentiate
                    , partialDeriv
                    , jacobian
                    , integrate
                    ) where

import Data.Array (array)

import Sci.Matrix

----------------------------------
------- Numerical calculus -------
----------------------------------

-- |A function for numerical differentiation up to a given tolerance.
differentiateTol :: (Num a, Ord a, Fractional a)
                 => (a -> a)
                 -> a
                 -> a
                 -> a
                 -> a
differentiateTol f a h tol
    | diff < tol = sndlim
    | otherwise = differentiateTol f a h' tol
    where
        lim = ((f (a + h)) - (f a)) / h
        h' = h / 2
        sndlim = ((f (a + h')) - (f a)) / h'
        diff = abs $ lim - sndlim

-- |Differentiate functions at a point. This is just a wrapper
-- around differentiateTol.
differentiate :: (Num a, Ord a, Fractional a) => (a -> a) -> a -> a
differentiate f a = differentiateTol f a 0.000001 1e-9

-- |Compute a partial derivative at a given point in n-space.
-- Note that the input function takes a list [x_0,...x_n],
-- and we take the partial derivative with respect to some
-- x_i given by the second argument (the index). This will
-- produce a runtime error if the dimensions are incorrect,
-- i.e., if as is not a list of the appropriate length for
-- the argument to f.
partialDeriv :: (Num a, Ord a, Fractional a)
             => ([a] -> a)
             -> Int
             -> [a]
             -> a
partialDeriv f i as = differentiate g a where
    a = as !! i
    g x = f $ (take i as) ++ (x : drop (i+1) as)

-- |Function to compute the Jacobian of a list of functions
-- at a given point in n-space. Produces a runtime error
-- if dimensions are incorrect.
jacobian :: (Num a, Ord a, Fractional a)
         => [[a] -> a]
         -> [a]
         -> Matrix a
jacobian fs xs = array ((0,0),(length fs - 1, length xs - 1)) $
                 [((i,j), partialDeriv (fs !! i) j xs)
                 | i <- [0..length fs - 1]
                 , j <- [0..length xs - 1]
                 ]

-- |Numerical integration up to a given tolerance.
integrateTol :: (Num a, Ord a, Fractional a, Enum a)
             => (a -> a)
             -> a
             -> a
             -> Int
             -> a
             -> a
integrateTol f a b divs tol
    | diff < tol = sndArea
    | otherwise = integrateTol f a b (divs * 2) tol
    where
        width = (b - a) / (fromIntegral divs)
        points = [a,a+width..b]
        partition = zip (init points) (tail points)
        sndPoints = [a,a + (width / 2)..b]
        sndPartition = zip (init sndPoints) (tail sndPoints)
        trapArea x y w = (x + y) * w / 2
        areas w p = map (\(x,y) -> trapArea (f x) (f y) w) p
        firstArea = sum $ areas width partition
        sndArea = sum $ areas (width / 2) sndPartition
        diff = abs $ firstArea - sndArea

-- |Integrate function f between bounds a and b.
-- This is just a wrapper around integrateTol.
integrate :: (Num a, Ord a, Fractional a, Enum a)
          => (a -> a)
          -> a
          -> a
          -> a
integrate f a b = integrateTol f a b 100 1e-7
