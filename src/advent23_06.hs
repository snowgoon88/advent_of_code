module Main where

import qualified MyParser as MP
import Data.Char ( digitToInt, isDigit )

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 06 - Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input06.txt"
  -- content <- readFile "Input23/test06_1.txt"

  let times = reverse $ MP.numList (MP.parseLabelList "Time" (head (lines content)))
  -- print times

  let records = reverse $ MP.numList (MP.parseLabelList "Distance" (head $ drop 1 (lines content)))
  -- print records

  let ranges = map nbWays (zip times records)
  -- print ranges

  let pRes = product ranges
  putStrLn $ "Answer 1> " ++ show pRes

  let bigTime = concatNb times
  let bigRecord = concatNb records
  -- putStrLn $ "nbWays (" ++ show bigTime ++ ", " ++ show bigRecord ++ ")"
  -- let minBigTime = minTime (bigTime, bigRecord) [1..]
  -- print $ "minTime=" ++ show minBigTime
  -- let maxBigTime = minTime (bigTime, bigRecord) (reverse [1..bigTime])
  -- print $ "maxTime=" ++ show maxBigTime

  let cRes = nbWays (bigTime, bigRecord)
  putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

-- distance travelled = (time-push) * push
dist :: Int -> Int -> (Int, Int)
dist time push = ((time - push) * push, push)

minTime :: (Int, Int) -> [Int] -> Int
minTime (time, record) range = snd $ head $ filter ((> record) . fst) (map (dist time) range)

nbWays :: (Int, Int) -> Int
nbWays (time, record) = maxPush - minPush + 1
  where minPush = minTime (time, record) [1..time]
        maxPush = minTime (time, record) (reverse [1..time])

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

concatNb :: [Int] -> Int
concatNb listDigit = read (concat (map show listDigit)) :: Int
