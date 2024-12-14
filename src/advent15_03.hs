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


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2015 - Day 03 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input15/input03.txt"
  -- content <- readFile "Input15/test03_1.txt"

  let houses = parseDir [(0, 0)] (0, 0) content
  let pRes = length houses
  putStrLn $ "Answer 1> " ++ show pRes

  let hSantaRobot = parseSanta [(0, 0)] ((0, 0), (0, 0)) content
  let cRes = length hSantaRobot
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Pos = (Int, Int)

parseDir :: [Pos] -> Pos -> String -> [Pos]
parseDir acc _ [] = acc
parseDir acc pos (d:ds)
  | elem newPos acc = parseDir acc newPos ds
  | otherwise       = parseDir (newPos:acc) newPos ds
  where
    newPos = move pos d

move :: Pos -> Char -> Pos
move (r, c) '^' = (r-1, c)
move (r, c) '<' = (r, c-1)
move (r, c) 'v' = (r+1, c)
move (r, c) '>' = (r, c+1)
move (r, c) other = error ("Move with [" ++ [other] ++ "]")


-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

parseSanta :: [Pos] -> (Pos, Pos) -> String -> [Pos]
parseSanta houses _ [] = houses
parseSanta houses (posS, posR) (d:ds)
  | elem newPos houses = parseRobot houses (newPos, posR) ds
  | otherwise = parseRobot (newPos:houses) (newPos, posR) ds
  where
    newPos = move posS d

parseRobot :: [Pos] -> (Pos, Pos) -> String -> [Pos]
parseRobot houses _ [] = houses
parseRobot houses (posS, posR) (d:ds)
  | elem newPos houses = parseSanta houses (posS, newPos) ds
  | otherwise = parseSanta (newPos:houses) (posS, newPos) ds
  where
    newPos = move posR d
