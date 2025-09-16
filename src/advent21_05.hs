{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
import Data.String.Utils (split)
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
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

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2021 - Day 05 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input21/input05.txt"
  -- content <- readFile "Input21/test05_1.txt"

  let overMap = overlappedLines (lines content)
  -- putStrLn $ "overMap = " ++ show overMap
  let pRes = Map.size $ Map.filter (>1) overMap
  putStrLn $ "Answer 1> " ++ show pRes

  let overMap2 = overlap2 (lines content)
  -- putStrLn $ "overMap = " ++ show overMap
  let cRes = Map.size $ Map.filter (>1) overMap2
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Point = (Int, Int)
type Line = (Point, Point)

-- read, sorting with the lowest 'x' first (usefull for diagonals)
parseLine :: String -> Line
parseLine line
  | x1 < x2 = ((x1, y1), (x2, y2))
  | otherwise = ((x2, y2), (x1, y1))
  where
    pts = split " -> " line
    [x1, y1] = map read (split "," (head pts))
    [x2, y2] = map read (split "," (head (drop 1 pts)))

type OverMap = Map.Map Point Int

-- filter Vertical and Horizontal Lines, transform to OverMap
-- and combine with (+)
overlappedLines :: [String] -> OverMap
overlappedLines input = Map.unionsWith (+)
  (map (toMap detailHVLine) (filter (\l -> isVert l || isHorizontal l) (map parseLine input)))

toMap :: (Line -> [Point]) -> Line -> OverMap
toMap detFunc line = Map.fromList (map (, 1) (detFunc line))

detailHVLine :: Line -> [Point]
detailHVLine ((x1, y1), (x2, y2)) = [(x,y) | x <- [min x1 x2 .. max x1 x2]
                                           , y <- [min y1 y2 .. max y1 y2]]

isVert :: Line -> Bool
isVert ((x1, _), (x2, _)) = x1 == x2
isHorizontal :: Line -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
isDiag :: Line -> Bool
isDiag ((x1, y1), (x2, y2)) = abs (x2 - x1) == abs (y2 - y1)

detailDiag :: Line -> [Point]
detailDiag ((x1, y1), (x2, y2))
  | y1 < y2 = [(x1+i,y1+i) | i <- [0 .. (x2 - x1)] ]
  | otherwise = [(x1+i, y1-i) | i <- [0 .. (x2 - x1)]]


overlap2 :: [String] -> OverMap
overlap2 input = Map.unionsWith (+) (mapHV ++ mapDiag)
  where
    allLines = map parseLine input
    mapHV = map (toMap detailHVLine) (filter (\l -> isVert l || isHorizontal l) allLines)
    mapDiag = map (toMap detailDiag) (filter isDiag allLines)
