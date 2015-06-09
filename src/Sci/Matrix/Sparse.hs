{-|
Module          : Sci.Matrix.Sparse
Description     : A module for computation with sparse matrices.
Maintainer      : nhalford@uchicago.edu
-}
module Sci.Matrix.Sparse ( sparse
                         , matrixToSparse
                         , sparseToMatrix
                         , transposeSparse
                         , addSparse
                         , multSparse
                         , diagSparse
                         , identSparse
                         , scaleSparse
                         , sparseMatrixExp
                         , SparseMatrix(..)
                         ) where

import Sci.Matrix
import Data.Array
import qualified Data.Map as M

-- |Data type for sparse matrices. This should be used instead
-- of Matrix whenever possible. The bound assumes (0,0) in the
-- top left corner, and values is a map between indices and
-- nonzero values of the matrix.
data SparseMatrix a = Sparse { bound :: (Int, Int)
                             , values :: (M.Map (Int, Int) a)
                             } deriving (Eq, Show)

----------------------------------
--------- Sparse matrices --------
----------------------------------

-- |Function to create a sparse matrix from list of indices
-- and list of values. Will fail if any index is out of bounds.
sparse :: (Eq a, Num a)
       => (Int, Int)
       -> [(Int, Int)]
       -> [a]
       -> Either String (SparseMatrix a)
sparse bs@(mxr,mxc) ixs vs
    | all (\(x,y) -> x <= mxr && y <= mxc) ixs
        = Right $ Sparse bs
          (M.fromList $ filter (\(_,v) -> v /= 0)
          $ zip ixs vs)
    | otherwise = Left "Index out of bounds"

-- |Convert a (dense) matrix to a sparse matrix
matrixToSparse :: (Num a, Eq a) => Matrix a -> SparseMatrix a
matrixToSparse m = Sparse sparseBounds
    (M.fromList $ filter (\(_,v) -> v /= 0) $ assocs m)
    where sparseBounds = (mxr - mnr, mxc - mnc)
          ((mnr,mnc),(mxr,mxc)) = bounds m


-- |Convert a sparse matrix to a (dense) matrix
sparseToMatrix :: Num a => SparseMatrix a -> Matrix a
sparseToMatrix (Sparse bs@(r,c) mMap) = array ((0,0),bs) $
    map getVal [(i,j) | i <- [0..r], j <- [0..c]]
    where getVal (x,y) = case M.lookup (x,y) mMap of
            Just v -> ((x,y),v)
            Nothing -> ((x,y),0)

-- |Compute thte transpose of a sparse matrix.
transposeSparse :: Num a => SparseMatrix a -> SparseMatrix a
transposeSparse a = Sparse { bound = bounds'
                           , values = values'
                           } where
    bounds' = flipTuple (bound a)
    values' = M.fromList $ map (\(k,v) ->
              (flipTuple k, v)) (M.toList $ values a)
    flipTuple (x,y) = (y,x)
        
-- |Add two sparse matrices. Will produce a monadic failure
-- if the matrices are of incompatible sizes.
addSparse :: Num a
          => SparseMatrix a
          -> SparseMatrix a
          -> Either String (SparseMatrix a)
addSparse a b
    | bound a == bound b = Right $ Sparse (bound a)
                           (M.unionWith (+) (values a) (values b))
    | otherwise = Left "Can't add two matrices of different sizes"

-- |Multiply two sparse matrices. Will produce a monadic failure
-- if the matrices are of incompatible sizes.
multSparse :: Num a
           => SparseMatrix a
           -> SparseMatrix a
           -> Either String (SparseMatrix a)
multSparse a b
    | ca == rb = Right matProd
    | otherwise = Left "Invalid sizes for matrix multiplication"
    where (ra,ca) = bound a
          (rb,cb) = bound b
          matProd = Sparse { bound = (ra, cb)
                           , values = vs
                           }
          -- new values
          vs = foldl (\m (k,v) ->
               M.insertWith (+) k v m) M.empty pairList
          pairList = [((i,k),v1*v2)
                     | ((i,j1),v1) <- M.toList (values a)
                     , ((j2,k),v2) <- M.toList (values b)
                     , j1 == j2
                     ]

-- |Create a diagonal sparse matrix from the list of
-- diagonal entries.
diagSparse :: (Eq a, Num a) => [a] -> SparseMatrix a
diagSparse vs = case result of
                Right x -> x
                Left _ -> Sparse { bound = (0,0)
                                 , values = M.fromList []
                                 }
    where result = sparse (n-1,n-1)
                   [(i,i) | i <- [0..n-1]] vs
          n = length vs

-- |Create an n x n identity matrix (sparse).
identSparse :: (Eq a, Num a) => Int -> SparseMatrix a
identSparse n = diagSparse (replicate n 1)

-- |Multiply a sparse matrix by a scalar.
scaleSparse :: Num a => SparseMatrix a -> a -> SparseMatrix a
scaleSparse m n = Sparse { bound = bound m
                         , values = fmap (*n) (values m)
                         }

-- |Fast exponentiation of sparse matrices. Will fail for
-- matrices that are not square.
sparseMatrixExp :: Num a
                => SparseMatrix a
                -> Int
                -> Either String (SparseMatrix a)
sparseMatrixExp m n
    | r /= c = Left "Can only exponentiate square matrices"
    | n == 1 = Right m
    | odd n = (sparseMatrixExp m (n-1)) >>= (multSparse m)
    | otherwise = (multSparse m m) >>=
                  flip sparseMatrixExp (n `div` 2)
    where (r,c) = bound m
