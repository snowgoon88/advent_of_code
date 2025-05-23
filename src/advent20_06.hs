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

-- ****** Data.String.Utils ** split, join, startswith, replace
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
import qualified Data.Set as Set
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
import Data.List.Extra (splitOn)
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
  putStrLn "** Advent 2020 - Day 06 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input06.txt"
  -- content <- readFile "Input20/test06_1.txt"

  let groups = splitOn "\n\n" content
  -- putStrLn $ "groups=" ++ show groups

  let pRes = sum (map answers groups)
  putStrLn $ "Answer 1> " ++ show pRes

  let answ2 = map answerAll groups
  -- putStrLn $ "answ2=" ++ show answ2

  let cRes = sum answ2
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

-- make a set, remove '\n' and count
answers :: String -> Int
answers group = Set.size (Set.delete '\n' $ Set.fromList group)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
allAnswerSet :: Set.Set Char
allAnswerSet = Set.fromList ['a'..'z']

answerAll :: String -> Int
answerAll groups = Set.size (foldl Set.intersection
                             allAnswerSet
                             (map Set.fromList (lines groups)))
