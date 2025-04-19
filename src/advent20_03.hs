{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC
import MyUtils (countTrue)

-- import Data.String.Utils ( split, join, startswith) --, startswith, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- fromJust, fromMaybe, catMaybes, isNothing, listToMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
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
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 03 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input03.txt"
  -- content <- readFile "Input20/test03_1.txt"

  let loc = gatherLocations 3 (lines content)
  -- print $ "loc=" ++ show loc

  let pRes = countTrue (== '#') loc
  putStrLn $ "Answer 1> " ++ show pRes

  -- let checkAlt = recLoc 0 "" 3 1 (tail (lines content))
  -- print $ "checkAlt=" ++ show (reverse checkAlt)

  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let nbTrees = map (\(h, v) -> countTrue (== '#') (recLoc 0 "" h v (tail (lines content)))) slopes
  -- print $ "nbTrees=" ++ show nbTrees
  let cRes = product nbTrees
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

-- generate the list of position on the various lines
genPos :: Int -> Int -> [Int]
genPos interval size = take size (iterate (+ interval) 0)

getLocation :: Int -> String -> Char
getLocation pos line = line !! (mod pos (length line))

gatherLocations :: Int -> [String] -> String
gatherLocations intervale allLines = map (uncurry getLocation) (zip positions allLines)
  where
    positions = genPos intervale (length allLines)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- using recursive with horStep et verStep, accumulate nbTrees in accNum
recLoc :: Int -> String -> Int -> Int -> [String] -> String
recLoc _ accLoc _ _ [] = accLoc
recLoc posH accLoc stepH stepV linesToParse = recLoc posNew (loc:accLoc) stepH stepV linesLeft
  where
    posNew = posH + stepH
    linesTmp = drop (stepV - 1) linesToParse
    loc = getLocation posNew (head linesTmp)
    linesLeft = tail linesTmp


-- *****************************************************************************
-- *********************************************************************** Smart
-- *****************************************************************************

-- smart way to read a 2D ascii grid ?? => uses lens !!!!
-- asciiGrid :: IndexedFold (Int, Int) String Char
-- asciiGrid = reindexed swap (lined <.> folded)


-- parseForest :: String -> Set (Int, Int)
-- parseForest = ifoldMapOf asciiGrid $ \xy c -> case c of
--     '#' -> S.singleton xy
--     _   -> S.empty
