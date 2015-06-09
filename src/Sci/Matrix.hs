{-|
Module          : Sci.Matrix
Description     : A module for computation with matrices.
Maintainer      : nhalford@uchicago.edu
-}
module Sci.Matrix ( showMatrix
                  , vectorFromList
                  , matrixFromLists
                  , linspace
                  , getColumn
                  , matrixToVector
                  , vectorToMatrix
                  , transpose
                  , (.+.)
                  , (.*.)
                  , matrixExp
                  , scale
                  , identMatrix
                  , diagMatrix
                  , Matrix
                  , Vector
                  ) where

import Data.Array
import Data.List (nub)

-- |A matrix data type.
type Matrix = Array (Int, Int)

-- |A vector data type.
type Vector = Array Int

-- |A variant of show that prints a matrix more neatly.
-- Mostly used for debugging, but can be useful.
showMatrix :: Show a => Matrix a -> String
showMatrix m = unlines $ map concat strResult where
    strResult = map (\x -> map (\y -> (show y) ++ "\t") x) result
    result = iter (c2 - c1 + 1) (elems m)
    iter _ [] = []
    iter n xs = first : iter n xs'
        where (first, xs') = splitAt n xs
    ((_,c1),(_,c2)) = bounds m

--------------------------------------------------------------
-- NOTE: Most computations are done in the Either monad     --
-- Computations should be strung together with Applicatives --
--------------------------------------------------------------

-- |Create a vector from a list.
vectorFromList :: [a] -> Vector a
vectorFromList vs = listArray (0,length vs - 1) vs

-- |Create a matrix from a list of list. Will only return
-- a matrix if all of the lists are of the same length;
-- otherwise returns a monadic failure.
-- We take the lists to be the rows of the matrix.
matrixFromLists :: [[a]] -> Either String (Matrix a)
matrixFromLists ls
    | length (nub $ map length ls) /= 1 = Left "Invalid lengths."
    | otherwise = Right $ listArray ((0,0),(r-1,c-1)) (concat ls)
    where r = length ls
          c = length $ head ls

-- |Create a column vector from the minimum value to
-- the maximum value with n evenly spaced points. This
-- is based on linspace from numpy. Note that linspace x x 1
-- will produce a monadic failure.
linspace :: (Num a, Fractional a, Enum a, Eq a)
         => a
         -> a
         -> Int
         -> Either String (Vector a)
linspace a b n
    | n <= 1 = Left "Invalid number of steps."
    | a == b = Left "Invalid range."
    | otherwise = Right $ vectorFromList $ [a,a+size..b]
    where size = (b - a) / (fromIntegral $ (n - 1))

-- |Extract given column of a matrix (as a vector). Will
-- produce a monadic failure if the desired column is out
-- of range.
getColumn :: Int -> Matrix a -> Either String (Vector a)
getColumn c m
    | c < c1 || c > c2 = Left "Invalid column."
    | otherwise = Right $ array (0,r2-r1) $
                map (\((x,_),v) -> (x,v)) $
                filter (\((_,x),_) -> (x == c)) (assocs m)
    where ((r1,c1),(r2,c2)) = bounds m

-- |Convert a matrix to a column vector. Will produce
-- a monadic failure if the matrix is not a single column.
matrixToVector :: Matrix a -> Either String (Vector a)
matrixToVector m
    | mnc /= mxc = Left "Invalid dimensions for vector."
    | otherwise = Right $ listArray (0,mxr-mnr) (elems m)
    where ((mnr,mnc),(mxr,mxc)) = bounds m

-- |Convert a column vector to a matrix.
vectorToMatrix :: Vector a -> Matrix a
vectorToMatrix v = listArray ((0,0),(mx-mn,0)) (elems v)
    where (mn,mx) = bounds v

-- |Compute the transpose of a matrix.
transpose :: Matrix a -> Matrix a
transpose a = array (mn,(c,r)) [((y,x),z)
                               | x <- [mr..r]
                               , y <- [mc..c]
                               , let z = a ! (x,y)
                               ]
    where (mn@(mr,mc),(r,c)) = bounds a
    
-- |Sum two matrices. Will produce a monadic failure if the matrices
-- are of incompatible sizes.
infixl 6 .+.
(.+.) :: Num a => Matrix a -> Matrix a -> Either String (Matrix a)
a .+. b
    | bounds a == bounds b = Right $ array (bounds a) $
        zipWith (\(i,x) (_,y) -> (i,x+y)) (assocs a) (assocs b)
    | otherwise = Left "Can't add two matrices of different sizes"

-- |Multiply two matrices. Will produce a monadic failure if the
-- matrices are of incompatible sizes.
infixl 7 .*.
(.*.) :: Num a => Matrix a -> Matrix a -> Either String (Matrix a)
a .*. b
    | mna == mnb && ca == rb = Right $ array (mna,(ra,cb)) $
        [((r,c),entry r c) | r <- [mnr..ra], c <- [mnc..cb]]
    | otherwise = Left "Invalid sizes for matrix multiplication"
    where
        (mna@(mnr,mnc),(ra,ca)) = bounds a
        (mnb,(rb,cb)) = bounds b
        entry r c = sum $ zipWith (*) (aRow r) (bCol c)
        aRow r = map snd $
                 filter (\((x,_),_) -> x == r) (assocs a)
        bCol c = map snd $
                 filter (\((_,x),_) -> x == c) (assocs b)

-- |Fast matrix exponentiation. Will produce a monadic failure
-- if the matrix is not square.
matrixExp :: Num a => Matrix a -> Int -> Either String (Matrix a)
matrixExp m n
    | r /= c = Left "Can only exponentiate square matrices"
    | n == 1 = Right m
    | odd n = (matrixExp m (n-1)) >>= (.*. m)
    | otherwise = (m .*. m) >>= flip matrixExp (n `div` 2)
    where (_,(r,c)) = bounds m

-- |Multiply a matrix by a scalar.
scale :: Num a => a -> Matrix a -> Matrix a
scale a m = array (bounds m) $ map (\(i,x) -> (i,a*x)) (assocs m)

-- |Create the n x n identity matrix
identMatrix :: Num a => Int -> Matrix a
identMatrix n = array ((0,0),(n-1,n-1)) $
    [((i,j),v)
    | i <- [0..n-1]
    , j <- [0..n-1]
    , let v = if i == j then 1 else 0
    ]

-- |Create an n x n diagonal matrix from a
-- list of its diagonal elements.
diagMatrix :: Num a => [a] -> Either String (Matrix a)
diagMatrix [] = Left "Empty list"
diagMatrix ds = Right $ array ((0,0),(n-1,n-1)) $
    [((i,j), v)
    | i <- [0..n-1]
    , j <- [0..n-1]
    , let v = if i == j then (ds !! i) else 0
    ] where
    n = length ds
