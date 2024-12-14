{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import Data.List ( sort, group ) --sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 02 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input02.txt"
  -- content <- readFile "Input24/test02_1.txt"

  let allReport = map parseLine (lines content)

  let diffs = map compDiff allReport
  let nonValids = filter (not . isValid. compDiff) allReport
  -- print $ "diffs=" ++ show diffs
  -- print $ "nonValids=" ++ show nonValids

  let pRes = length diffs - length nonValids
  putStrLn $ "Answer 1> " ++ show pRes

  let stillValid = map betterValid nonValids
  let nbStillValid = length (filter id stillValid)
  -- print $ "stillValid=" ++ show stillValid
  -- print $ "nbStillValid=" ++ show nbStillValid

  let cRes = pRes + nbStillValid
  putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

parseLine :: String -> [Int]
parseLine line = map read (split " " line)

compDiff :: [Int] -> [Int]
compDiff report = map (\(x, y) -> x - y) (zip report (drop 1 report))

isValid :: [Int] -> Bool
isValid diff = (all (>0) diff || all (<0) diff) && all ((<4) . abs) diff

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

removeElemAt :: [Int] -> Int -> [Int]
removeElemAt l idx = take idx l ++ drop (idx+1) l

betterValid :: [Int] -> Bool
betterValid l = any id $ map (isValid . compDiff . removeElemAt l) [0..(length l -1)]
