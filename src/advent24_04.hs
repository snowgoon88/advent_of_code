{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import qualified MyParser as MP
import MyParser ( GridMap, readGrid, chunks )
-- import Data.List ( sort, group ) --sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( split ) --, join, replace, split )
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
  putStrLn "** Advent 2024 - Day 04 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input04.txt"
  -- content <- readFile "Input24/test04_1.txt"

  let gridmap = readGrid (lines content)
  print $ "size=" ++ show (fst gridmap)

  let xpos = Map.keys $ Map.filter (== 'X') (snd gridmap)
  -- print $ "xpos=" ++ show xpos

  -- let check01 = checkLetter gridmap "XMAS" (0,5) (0,1)
  -- print $ "check01=" ++ show check01

  -- let check02 = map (checkLetter gridmap "XMAS" (2, 2)) allDir
  -- print $ "check02=" ++ show check02

  let checkAll = concat $ map (\p -> map (checkLetter gridmap "XMAS" p ) allDir) xpos
  -- print $ "checkAll=" ++ show checkAll

  let pRes = length $ filter id checkAll
  putStrLn $ "Answer 1> " ++ show pRes

  let apos = Map.keys $ Map.filter (== 'A') (snd gridmap)
  print $ "len apos=" ++ show (length apos)



  -- let cp01 = map (checkPattern gridmap "MSMS" (1, 2)) allMSPatterns
  -- print $ "cp01=" ++ show cp01

  let getAllPos = concat $ map (\p -> map (\pat -> checkPatternD gridmap "MSMS" p pat pat) allMSPatterns) apos
  -- print $ "getAllPos=" ++ show getAllPos

  let toCheck = filter (\(b,p,lp) -> b) getAllPos
  -- print $ "toCheck=" ++ show toCheck

  -- let displayCross (_, pos, dirs) = do
  --       print $ "-- Pos=" ++ show pos ++ "  => " ++ show dirs
  --       putStrLn $ (chunks 3 $ niceSubGrid gridmap allPos pos)

  -- mapM_ displayCross toCheck


  let checkAllXCross = map (\p -> map (checkPattern gridmap "MSMS" p ) allMSPatterns) apos
  -- -- print $ "checkAllXCross=" ++ show checkAllXCross

  let okPos = map (any id) checkAllXCross
  -- -- print $ "okPos=" ++ show okPos


  let cRes = length $ filter id okPos
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Pos = (Int, Int)
type Size = (Int, Int)
allDir = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1], r /= 0 || c/= 0]
allPos = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1]]

addDir :: Pos -> Pos -> Pos
addDir (r, c) (dr, dc) = (r+dr, c+dc)

isValidPos :: Size -> Pos -> Bool
isValidPos (rMax, cMax) (r, c) = r >= 0 && c >= 0 && r < rMax && c < cMax

checkLetter :: GridMap -> String -> Pos -> Pos -> Bool
checkLetter _ [] _ _ = True
checkLetter (size, grid) (letter:ls) pos dir
  | not (isValidPos size pos) = False
  | otherwise = (gridLetter == letter) && checkLetter (size, grid) ls (addDir pos dir) dir
  where
    gridLetter = grid Map.! pos

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
allMSPatterns = [--[(-1, 0), (1, 0), (0, 1), (0, -1)],
               [(-1, 1), (1, -1), (1, 1), (-1, -1)],
               --[(0, 1), (0, -1), (1, 0), (-1, 0)],
               [(1, 1), (-1, -1), (1, -1), (-1, 1)],
               --[(1, 0), (-1, 0), (0, -1), (0, 1)],
               [(1, -1), (-1, 1), (-1, -1), (1, 1)],
               --[(0, -1), (0, 1), (-1, 0), (1, 0)],
               [(-1, -1), (1, 1), (-1, 1), (1, -1)]]

checkPattern :: GridMap -> String -> Pos -> [Pos] -> Bool
checkPattern _ [] _ [] = True
checkPattern (size, grid) (letter:ls) pos (dir:ds)
  | not (isValidPos size (addDir pos dir)) = False
  | otherwise = (gridLetter == letter) && checkPattern (size, grid) ls pos ds
  where
    gridLetter = grid Map.! (addDir pos dir)

checkPatternD :: GridMap -> String -> Pos -> [Pos] -> [Pos] -> (Bool, Pos, [Pos])
checkPatternD _ [] pos [] crossPos = (True, pos, crossPos)
checkPatternD (size, grid) (letter:ls) pos (dir:ds) crossPos
  | not (isValidPos size (addDir pos dir)) = (False, pos, crossPos)
  | otherwise = if gridLetter == letter
                   then checkPatternD (size, grid) ls pos ds crossPos
                   else (False, pos, crossPos)
  where
    gridLetter = grid Map.! (addDir pos dir)

niceSubGrid :: GridMap -> [Pos] -> Pos -> String
niceSubGrid _ [] _ = ""
niceSubGrid (size, grid) (dir:ds) pos
  | isValidPos size (addDir pos dir) = (grid Map.! (addDir pos dir)) : niceSubGrid (size, grid) ds pos
  | otherwise = '#' : niceSubGrid (size, grid) ds pos
