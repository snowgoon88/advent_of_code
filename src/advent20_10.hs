{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
-- ****** Data.List.Extra: splitOn
import Data.List.Extra (sort)
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 10 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input10.txt"
  -- content <- readFile "Input20/test10_1.txt"
  -- content <- readFile "Input20/test10_2.txt"

  let adaptors = map read (lines content) :: [Int]
  -- putStrLn $ "adaptors=" ++ show adaptors

  let (c1,c2,c3) = chain1 adaptors
  -- putStrLn $ "chain1=" ++ show (c1, c2, c3)
  let pRes = c1 * c3
  putStrLn $ "Answer 1> " ++ show pRes

  let sortA = [0] ++ sort adaptors ++ [maximum adaptors +3]
  putStrLn $ "fullchain=" ++ show sortA
  let interv = getIntervales adaptors
  putStrLn $ "interv=" ++ show interv

  -- let maxLen = maximum (map snd interv)
  -- putStrLn $ "maxLen=" ++ show maxLen

  let cRes = product $ map nbCom interv
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

chain1 :: [Int] -> (Int, Int, Int) --, [Int], [Int])
chain1 adaptors = (ones, twos, threes) --, sorta, differences)
  where
    maxJ = maximum adaptors + 3
    sorta = sort (0:maxJ:adaptors)
    differences = map (uncurry (-)) $ zip (tail sorta) sorta
    ones = length $ filter (== 1) differences
    twos = length $ filter (== 2) differences
    threes = length $ filter (== 3) differences

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- cannot change order when the diff is 3
-- no gap of length 2
-- how many combination for every interval of length 2 between 2 limits of 3 gaps
-- ex1: (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
--              |<      >|   |<     >|
--               4 combo      2 combs                    => 8 possibilities

-- with 0, 1, 2, 3, 4
-- 01234, 0234, 0134, 0124, 034, 024, 014

-- return (start, length) of sequence of diff of 1
getIntervales :: [Int] -> [(Int, Int)]
getIntervales adaptors = scan3 [] 0 0 sorta
  where
    maxJ = maximum adaptors + 3
    sorta = sort (0:maxJ:adaptors)
    scan3 :: [(Int, Int)] -> Int -> Int -> [Int] -> [(Int, Int)]
    scan3 acc _ _ [_] = acc
    scan3 acc curLen start (x:y:ys)
      | y-x == 1 = scan3 acc (curLen+1) start (y:ys)
      | y-x == 3 = scan3 ((start, curLen):acc) 0 y (y:ys)
      | otherwise = scan3 acc curLen start (y:ys)

-- from a length of sequence of 1's, combute the valid number of allowed combinations
-- to reach the extremum of the interval.
nbCom :: (Int, Int) -> Int
nbCom (_, 4 ) = 7
nbCom (_, 3 ) = 4
nbCom (_, 2 ) = 2
nbCom _ = 1
