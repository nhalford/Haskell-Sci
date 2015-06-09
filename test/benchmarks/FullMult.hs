-- A simple test for multiplying two large sparse matrices.
module Main where

import Sci.Matrix
import Sci.DataProcessing

main :: IO ()
main = do
    -- create the first matrix
    m1 <- loadFile "rand1.txt" "#" ' ' 0 [0..99]
    m2 <- loadFile "rand2.txt" "#" ' ' 0 [0..99]
    -- multiply
    let p = m1 .*. m2 :: Either String (Matrix Int)
    mapM_ (writeFile "out.txt") $ fmap showMatrix p
--    print p
    return ()
