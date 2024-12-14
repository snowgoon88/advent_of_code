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
import Data.String.Utils ( split ) --, join, replace, split )
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
  putStrLn "** Advent 2025 - Day 02 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input15/input02.txt"
  -- content <- readFile "Input15/test02_1.txt"

  let pRes = sum $ map surface (map parseLine (lines content))
  putStrLn $ "Answer 1> " ++ show pRes

  let cRes = sum $ map ribbon (map parseLine (lines content))
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

parseLine :: String -> (Int, Int, Int)
parseLine line = (read $ tok !! 0, read $ tok !! 1, read $ tok !! 2)
  where tok = split "x" line

surface :: (Int, Int, Int) -> Int
surface (l, w, h) = 2 * sum surf + minimum surf
  where surf = [l*h, h*w, w*l]

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

ribbon :: (Int, Int, Int) -> Int
ribbon (l, w, h) = (l*w*h) + 2 * minimum face
  where face = [l+h, h+w, w+l]
