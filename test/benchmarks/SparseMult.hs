-- A simple test for multiplying two large sparse matrices.
module Main where

import Sci.Matrix
import Sci.Matrix.Sparse

import Data.Array (listArray)

main :: IO ()
main = do
    -- create the first matrix
    let m1 = case diagMatrix [1..100] of
             Right x -> x
             Left _ -> listArray ((0,0),(0,0)) []
    -- create the second matrix
    let m2 = case diagMatrix [101..200] of
             Right x -> x
             Left _ -> listArray ((0,0),(0,0)) []
    -- convert to sparse matrices
    let s1 = matrixToSparse m1
    let s2 = matrixToSparse m2
    -- multiply
    let p = multSparse s1 s2
    print p
    return ()
