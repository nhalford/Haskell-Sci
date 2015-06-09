{-|
Module          : Sci.DataProcessing
Description     : A module for processing scientific data.
Maintainer      : nhalford@uchicago.edu
-}
module Sci.DataProcessing (loadFile) where

import Sci.Matrix

import Data.Array
import Data.List (isPrefixOf)
import Data.Char (isSpace)

---------------------------------
-------- Data Processing --------
---------------------------------

-- |Extract elements at given indices from a list.
-- Used as a helper function in loadFile.
elemsAtIndices :: [Int] -> [a] -> [a]
elemsAtIndices ixs l = map (l !!) $ filter
                       (flip elem [0..(length l)-1]) ixs

-- |Remove comments from a string. First argument is
-- the string that comments start with
-- used as a helper function in loadFile
stripComments :: String -> String -> String
stripComments _ "" = ""
stripComments "" s = s
stripComments comm s@(x:xs)
    | comm `isPrefixOf` s = ""
    | otherwise = x : stripComments comm xs

-- |Generalized version of words, used as a helper function
-- in loadFile.
splitOnChar :: Char -> String -> [String]
splitOnChar _ "" = []
splitOnChar c s
    | isSpace c = words s
    | otherwise = case rest of [] -> [first]
                               (_:xs) -> first : splitOnChar c xs
    where (first, rest) = break (== c) s

-- |Read a file at a given path and load it into a matrix.
-- Arguments:
--    path = path to file
--    comm = string indicating the beginning of a comment
--    del = delimiter. If whitespace, then all whitespace will
--          be treated as the delimiter.
--    skip = number of rows to skip from the beginning of the
--           file. This DOES include comment lines.
--    cols = indices of columns to load, L to R, starting at 0.
-- This is based on numpy.loadtxt.
loadFile :: Read a
         => String
         -> String
         -> Char
         -> Int
         -> [Int]
         -> IO (Matrix a)
loadFile path comm del skip cols = do
    contents <- readFile path
    -- get lines, remove comments, skip over desired lines
    let linesToProcess = filter (/= "") $
                         map (stripComments comm) $
                         drop (max 0 skip) (lines contents)
    -- create list of our data (as strings). Each entry of
    -- the list is a list corresponding to one row of the
    -- resulting matrix
    let dataLists = map (elemsAtIndices cols) $
                    map (splitOnChar del) linesToProcess
    let firstMatrix = matrixFromLists $ map (map read) dataLists
    let dataMatrix = case firstMatrix of
                     Right x -> x
                     Left _ -> array ((0,0),(0,0)) []
    return dataMatrix
