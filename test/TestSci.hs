-- TestSci.hs
-- Author: Noah Halford
-- June 9, 2015
-- A test suite for a scientific computing library.

module Main (main) where

import Sci.Calculus
--import Sci.DataProcessing
import Sci.Matrix
import Sci.Matrix.Sparse

import Data.Array
import qualified Data.Map as M

import Test.Hspec

main :: IO ()
main = runTests

-- run all of our tests
runTests :: IO ()
runTests = hspec $ describe "Testing scinetific computing library" $ do
    describe "Testing calculus functions" $ do
        it "computes exponential derivatives correctly" $ do
            ((differentiate exp 4) `approx` (exp 4)) `shouldBe` True
        it "computes polynomial derivatives correctly" $ do
            ((differentiate testPoly 3) `approx` 108) `shouldBe` True
        it "computes logarithmic derivatives correctly" $ do
            ((differentiate log 10) `approx` 0.1) `shouldBe` True
        it "computes exponential integrals correctly" $ do
            ((integrate exp 0 4) `approx` ((exp 4) - 1)) `shouldBe` True
        it "computes polynomial integrals correctly" $ do
            ((integrate testPoly 0 3) `approx` (243 / 5))
                `shouldBe` True
        it "computes partial derivatives correctly" $ do
            ((partialDeriv (\[x,y] -> y*x*x) 0 [5,3])
                `approx` 30) `shouldBe` True
    describe "Testing matrix operations" $ do
        it "computes linspace correctly" $ do
            linspace 0.1 10 100 `shouldBe` (Right linspaceTest)
        it "creates vectors and extracts columns correctly" $ do
            (getColumn 0 testMatrixFull) `shouldBe`
                Right firstFullCol
        it "transposes matrices correctly" $ do
            (transpose testMatrixFull) `shouldBe` testMatrixFullTrans
        it "adds matrices correctly" $ do
            (testMatrixDiag .+. testMatrixOnes) `shouldBe`
                Right testMatrixSum
        it "multiplices matrices correctly" $ do
            (testMatrixDiag .*. testMatrixOnes) `shouldBe`
                Right testMatrixProd
        it "exponentiates matrices correctly" $ do
            (matrixExp testMatrixDiag 5) `shouldBe`
                Right testMatrixExp
        it "scales matrices correctly" $ do
            fmap (scale 3) (testMatrixSum .+. testMatrixProd)
                `shouldBe` Right testMatrixScale
        it "computes the identity correctly" $ do
            identMatrix 3 `shouldBe` testMatrixIdent
        it "computes diagonal matrices correctly" $ do
            diagMatrix [3,2,4] `shouldBe` Right testMatrixDiag
    describe "Testing sparse matrices" $ do
        it "creates sparse matrices correctly" $ do
            (sparse (2,2) [(0,1),(0,2),(1,1),(1,2),(2,0)] [1,2,2,1,1])
                `shouldBe` Right testSparse1
        it "converts from matrix to sparse matrix correctly" $ do
            matrixToSparse testSparseMat1 `shouldBe` testSparse1
        it "converts from sparse to matrix correctly" $ do
            sparseToMatrix testSparse2 `shouldBe` testSparseMat2
        it "takes transposes of sparse matrices correctly" $ do
            transposeSparse testSparse2 `shouldBe` testSparse2Trans
        it "adds sparse matrices correctly" $ do
            addSparse testSparse1 testSparse2 `shouldBe`
                Right testSparseAdd
        it "multiplies sparse matrices correctly" $ do
            multSparse testSparse1 testSparse2 `shouldBe`
                Right testSparseMult
        it "creates  diagonal sparse matrices correctly" $ do
            diagSparse [3,2,4] `shouldBe` testSparseDiag
        it "scales sparse matrices correctly" $ do
            scaleSparse testSparse1 5 `shouldBe` testSparseScale
        it "exponentiates sparse matrices correctly" $ do
            sparseMatrixExp testSparseDiag 5 `shouldBe`
                Right testSparseExp

-- check if two numbers are approximately equal
-- (within 0.01%). This is necessary for testing because
-- floating point errors are likely to reduce accuracy
-- for functions that work.
approx :: Double -> Double -> Bool
approx x y = abs ((x - y) / x) < 0.0001

-- for testing polynomial differentation/exponentiation
testPoly :: Double -> Double
testPoly x = x * x * x * x

-- expected result of our linspace test
linspaceTest :: Vector Double
linspaceTest = listArray (0,99) [0.1,0.2..10]


-- some matrices used for testing

testMatrixFull :: Matrix Double
testMatrixFull = listArray ((0,0),(2,2)) [1..9]

firstFullCol :: Vector Double
firstFullCol = vectorFromList [1,4,7]

testMatrixFullTrans :: Matrix Double
testMatrixFullTrans = listArray ((0,0),(2,2)) $
    [1,4,7,2,5,8,3,6,9]

testMatrixOnes :: Matrix Double
testMatrixOnes = listArray ((0,0),(2,2)) (replicate 9 1)

testMatrixIdent :: Matrix Double
testMatrixIdent = listArray ((0,0),(2,2)) $
    [1,0,0,0,1,0,0,0,1]

testMatrixDiag :: Matrix Double
testMatrixDiag = listArray ((0,0),(2,2)) $
    [3,0,0,0,2,0,0,0,4]

-- fifth power of testMatrixDiag
testMatrixExp :: Matrix Double
testMatrixExp = listArray ((0,0),(2,2)) $
    [243,0,0,0,32,0,0,0,1024]

-- sum of testMatrixDiag and testMatrixOnes
testMatrixSum :: Matrix Double
testMatrixSum = listArray ((0,0),(2,2)) [4,1,1,1,3,1,1,1,5]

-- product of testMatrixDiag and testMatrixOnes
testMatrixProd :: Matrix Double
testMatrixProd = listArray ((0,0),(2,2)) [3,3,3,2,2,2,4,4,4]

-- (testMatrixSum + testMatrixProd) scaled by 3
testMatrixScale :: Matrix Double
testMatrixScale = listArray ((0,0),(2,2)) $
    [21,12,12,9,15,9,15,15,27]

-- matrices for testing sparse operations
-- most are analogs to the above for regular matrices

testSparseMat1 :: Matrix Double
testSparseMat1 = listArray ((0,0),(2,2)) $ [0,1,2,0,2,1,1,0,0]

testSparseMat2 :: Matrix Double
testSparseMat2 = listArray ((0,0),(2,2)) $ [0,2,0,0,0,3,0,5,0]

-- the same as above, but sparse-ified
testSparse1 :: SparseMatrix Double
testSparse1 = Sparse { bound = (2,2),
              values = M.fromList [((0,1),1),((0,2),2)
                                  ,((1,1),2),((1,2),1)
                                  ,((2,0),1)]
              }

testSparse2 :: SparseMatrix Double
testSparse2 = Sparse { bound = (2,2),
              values = M.fromList [((0,1),2),((1,2),3),((2,1),5)]
              }

-- transpose of testSparse2
testSparse2Trans :: SparseMatrix Double
testSparse2Trans = Sparse { bound = (2,2),
                   values = M.fromList [((1,0),2),((2,1),3),((1,2),5)]
                   }

-- diagonal sparse matrix
testSparseDiag :: SparseMatrix Double
testSparseDiag = Sparse { bound = (2,2),
                 values = M.fromList [((0,0),3), ((1,1),2), ((2,2),4)]
                 }

-- testSparse1 scaled by 5
testSparseScale :: SparseMatrix Double
testSparseScale = Sparse { bound = (2,2),
                  values = M.fromList [((0,1),5),((0,2),10)
                                      ,((1,1),10),((1,2),5)
                                      ,((2,0),5)]
                  }

-- testSparse1 + testSparse2
testSparseAdd :: SparseMatrix Double
testSparseAdd = Sparse { bound = (2,2),
                values = M.fromList [((0,1),3),((0,2),2)
                                    ,((1,1),2),((1,2),4)
                                    ,((2,0),1),((2,1),5)]
                }

-- testSparse1 * testSparse2
testSparseMult :: SparseMatrix Double
testSparseMult = Sparse { bound = (2,2),
                 values = M.fromList [((0,1),10),((0,2),3)
                                     ,((1,1),5),((1,2),6)
                                     ,((2,1),2)]
                 }

-- fifth power of testSparseDiag
testSparseExp :: SparseMatrix Double
testSparseExp = Sparse { bound = (2,2),
                values = M.fromList [((0,0),243), ((1,1),32), ((2,2),1024)]
                }
