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
  putStrLn "** Advent 2015 - Day 01 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input15/input01.txt"
  -- content <- readFile "Input15/test01_1.txt"

  let pRes = parseParent 0 content
  putStrLn $ "Answer 1> " ++ show pRes

  let cRes = parseLevel 0 (zip content [1..])
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

parseParent :: Int -> String -> Int
parseParent level [] = level
parseParent level ('(':ls) = parseParent (level+1) ls
parseParent level (')':ls) = parseParent (level-1) ls
parseParent level (l:ls) = parseParent level ls

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

parseLevel :: Int -> [(Char, Int)] -> Int
parseLevel level [] = error "Should find before"
parseLevel (-1) ((_, step):ls) = (step - 1)
parseLevel level (('(', _):ls) = parseLevel (level+1) ls
parseLevel level ((')', _):ls) = parseLevel (level-1) ls
parseLevel level (l:ls) = parseLevel level ls
