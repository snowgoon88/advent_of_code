{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC

-- import Data.String.Utils ( split, join, startswith) --, startswith, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
import Data.Maybe ( fromJust, listToMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing, listToMaybe )
import qualified Data.IntSet      as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( find, sortOn, groupBy ) -- sort, group, find, delete, sortOn, groupBy ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
-- import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
import Debug.Trace ( trace ) -- trace :: String > a -> a
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug :: c -> String -> c
debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 01 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input01.txt"
  -- content <- readFile "Input20/test01_1.txt"

  let numbers = map read (lines content) :: [Int]
  print numbers
  let pRes = checkExpense 2020 numbers
  putStrLn $ "Answer 1> " ++ show pRes


  let small = filter (<1500) numbers
  print $ "small=" ++ show small
  let altRes = cartesianTriple 2020 numbers
  print $ "altRes=" ++ show altRes
  let cRes = product (head altRes)

  --not working let cRes = checkTriple 2020 numbers
  putStrLn $ "Answer 2> " ++ show cRes

  let part2 = (knapsack 3 2020 . IS.fromList) numbers
  putStrLn $ "Answer SMART> " ++ show (product (fromJust part2))

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
checkExpense :: Int -> [Int] -> Maybe Int
checkExpense _ [] = Nothing
checkExpense val (x:xs) = case checkList val x xs of
  Nothing -> checkExpense val xs
  Just res -> Just res
checkList :: Int -> Int -> [Int] -> Maybe Int
checkList _ _ [] = Nothing
checkList val x (y:ys)
  | x+y == val = Just (x*y)
  | otherwise  = checkList val x ys

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- could also only look with all values < 1000, then test on cartesians product
cartesianTriple :: Int -> [Int] -> [[Int]]
cartesianTriple val xs = filter ((== val) . sum) [[x, y, z] | x <- kept,y <- kept,z <- kept]
  where kept = filter (<1500) xs

-- *****************************************************************************
-- *********************************************************************** Smart
-- *****************************************************************************

{- Using a set if Int (ordered and easy to split)
   and the fact that list are kind of Monads (the do usage of lists)
-}
-- | Given a number n of items and a goal sum and a set of numbers to
-- pick from, finds the n numbers in the set that add to the goal sum.
knapsack
    :: Int              -- ^ number of items n to pick
    -> Int              -- ^ goal sum
    -> IS.IntSet        -- ^ set of options
    -> Maybe [Int]      -- ^ resulting n items that sum to the goal
knapsack 0 _    _  = Nothing
knapsack 1 goal xs
    | goal `IS.member` xs = Just [goal]
    | otherwise           = Nothing
knapsack n goal xs = listToMaybe $ do
    x <- IS.toList xs
    let goal'   = goal - x
        (_, ys) = IS.split x xs
    case knapsack (n - 1) goal' ys of
      Nothing -> mempty
      Just rs -> pure (x:rs)
