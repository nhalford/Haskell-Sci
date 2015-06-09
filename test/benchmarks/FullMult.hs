-- A simple test for multiplying two large sparse matrices.
module Main where

import Sci.Matrix

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
    -- multiply
    let p = m1 .*. m2 :: Either String (Matrix Int)
    mapM_ (writeFile "out.txt") $ fmap showMatrix p
--    print p
    return ()
