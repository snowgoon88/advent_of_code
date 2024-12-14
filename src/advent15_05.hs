{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
-- import Data.List ( sort ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust, catMaybes )
-- import Control.Monad ( fold )
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace

-- import qualified Data.Hash.MD5 as MD5


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2015 - Day 05 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input15/input05.txt"
  -- content <- readFile "Input15/test05_1.txt"

  -- let ex = ["ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"]
  -- print $ "exVoy=" ++ show (map (countVoy 3 0) ex)
  -- print $ "exTwice=" ++ show (map twice ex)
  -- print $ "exBad=" ++ show (map (not . isBad) ex)

  let pRes = length (filter isNice (lines content))
  putStrLn $ "Answer 1> " ++ show pRes

  -- let exAdv = ["qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg", "ieodomkazucvgmuy" ]
  -- print $ "exAdvTrip=" ++ show (map hasTriplet exAdv)
  -- print $ "exAdvRepeat=" ++ show (map repeated exAdv)

  let cRes = length (filter isNiceAdv (lines content))
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
isNice :: String -> Bool
isNice str = (countVoy 3 0 str) && (twice str) && (not (isBad str))

countVoy :: Int -> Int -> String -> Bool
countVoy nbMin curNb []
  | curNb >= nbMin = True
  | otherwise = False
countVoy nbMin curNb (c:cs)
  | curNb >= nbMin = True
  | elem c ['a', 'e', 'o', 'i', 'u'] = countVoy nbMin (curNb+1) cs
  | otherwise = countVoy nbMin curNb cs

twice :: String -> Bool
twice [] = False
twice [c] = False
twice (c1:c2:cs)
  | c1 == c2 = True
  | otherwise = twice (c2:cs)


isBad :: String -> Bool
isBad [] = False
isBad ('a':'b':cs) = True
isBad ('c':'d':cs) = True
isBad ('p':'q':cs) = True
isBad ('x':'y':cs) = True
isBad (c:cs) = isBad cs
-- isBad b = error ("isBad cannot deal with " ++ show b)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

isNiceAdv :: String -> Bool
isNiceAdv str = (hasTriplet str) && (repeated str)

hasTriplet :: String -> Bool
hasTriplet (c1:c2:c3:cs)
  | c1 == c3 = True
  | otherwise = hasTriplet (c2:c3:cs)
hasTriplet [c1, c2] = False

repeated [c1, c2] = False
repeated (c1:c2:cs) = (doubled c1 c2 cs) || repeated (c2:cs)

doubled _ _  [c1] = False
doubled p1 p2 (c1:c2:cs)
  | c1 == p1 && c2 == p2 = True
  | otherwise = doubled p1 p2 (c2:cs)
