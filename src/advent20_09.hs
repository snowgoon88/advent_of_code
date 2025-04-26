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
  putStrLn "** Advent 2020 - Day 09 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input09.txt"
  -- content <- readFile "Input20/test09_1.txt"

  let serie = map read (lines content) :: [Int]

  let pRes = checkCode 25 serie
  putStrLn $ "Answer 1> " ++ show pRes

  let cRes = findCumsum pRes serie
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

xmasCode :: Int -> [Int] -> Maybe Int
xmasCode preambule nums = if any (checkComplementary prev code) prev
  then Just code
  else Nothing
  where
    prev = take preambule nums
    code = head (drop preambule nums)
    checkComplementary :: [Int] -> Int -> Int -> Bool
    checkComplementary candidates total c1 = (total - c1) /= c1 &&
      (elem (total - c1) candidates)

checkCode :: Int -> [Int] -> Int
checkCode preambule nums = case xmasCode preambule nums of
  Nothing -> head (drop preambule nums)
  Just _  -> checkCode preambule $ tail nums

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
headMaybe :: [a] -> Maybe a
headMaybe x = if length x > 0
  then Just $ head x
  else Nothing

doCumSum :: [Int] -> [(Int, Int, Int)]
doCumSum (x:y:ys) = foldl op [(x+y, min x y, max x y)] ys
  where
    op :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
    op acc n = (csum+n, min minNum n, max maxNum n) : acc
      where
        (csum, minNum, maxNum) = head acc
doCumSum _ = []

cumsum :: Int -> [Int] -> Maybe (Int, Int, Int)
cumsum _ [] = Nothing
cumsum _ [_] = Nothing
cumsum total (x:y:ys) = headMaybe $ dropWhile (\(cs,_,_) -> cs > total)
  (foldl op [(x+y, min x y, max x y)] ys)
  where
    op :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
    op acc n = (csum+n, min minNum n, max maxNum n) : acc
      where
        (csum, minNum, maxNum) = head acc

findCumsum :: Int -> [Int] -> Maybe (Int, Int, Int)
findCumsum goal nums = case cumsum goal nums of
  Nothing -> Nothing
  Just (csum, minN, maxN) -> if csum == goal
    then Just (minN + maxN, minN, maxN)
    else findCumsum goal $ tail nums
