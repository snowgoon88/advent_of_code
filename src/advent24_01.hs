{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import Data.List ( sort, group ) --sortOn, groupBy ) --, find ) --group ) --, sort, , sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split ) --, join ) --replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
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
  putStrLn "** Advent 2024 - Day 01 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input01.txt"
  -- content <- readFile "Input24/test01_1.txt"

  let parsed = map parseLine (lines content)
  -- print $ "parsed=" ++ show parsed

  let l1 = map fst parsed
  let l2 = map snd parsed

  let distances = computeDist l1 l2
  -- print $ "distances=" ++ show distances

  let pRes = sum distances
  putStrLn $ "Answer 1> " ++ show pRes

  let left = prepList l1
  let rightMap = Map.fromList $ prepList l2
  print $ "left=" ++ show left
  print $ "rightMap=" ++ show rightMap

  let simil = similarities left rightMap
  print $ "simil=" ++ show simil

  let cRes = sum simil
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

parseLine :: String -> (Int, Int)
parseLine line = (read $ head tokens, read $ tokens !! 1)
  where
    tokens = split "   " line

computeDist :: [Int] -> [Int] -> [Int]
computeDist l1 l2 = map (\(x, y) -> abs(x - y)) (zip (sort l1) (sort l2))

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- for a Value, its number
type ValueMap = Map.Map Int Int

-- return list of (value, nbTimes)
prepList :: [Int] -> [(Int, Int)]
prepList l = map (\sublist -> (head sublist, length sublist)) (group (sort l))

similarities :: [(Int, Int)] -> ValueMap -> [Int]
similarities left rightMap = map (computeSimil rightMap) left
  where
    computeSimil :: ValueMap -> (Int, Int) -> Int
    computeSimil valueMap (value, times) = case Map.lookup value valueMap of
      Just n -> times * (value * n)
      Nothing -> 0
