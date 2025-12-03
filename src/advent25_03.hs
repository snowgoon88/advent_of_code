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
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
import Data.Char ( digitToInt )
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.Foldable ( foldl' )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List ( elemIndex )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)
import Data.List.Utils (strFromAL)

-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug :: c -> String -> c
-- debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2025 - Day 03 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let banks = map parseBank (lines content)
  let maxis = map (findMaximum 9 ) banks
  putStrLn $ "maxis=" ++ show maxis
  let pRes = sum maxis
  putStrLn $ "Answer 1> " ++ show pRes

  let joltages = map (findMaximumWithin 9 11 0) banks
  putStrLn $ "joltages=" ++ show joltages
  let cRes = sum joltages
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

parseBank :: String -> [Int]
parseBank line = map digitToInt line

findMaximum :: Int -> [Int] -> Int
findMaximum 0 _ = 0
findMaximum n str = case elemIndex n str of
  Just idxN -> if length afterN > 0 then n * 10 + maximum afterN else findMaximum (n-1) str
    where
      afterN = drop (idxN+1) str
  Nothing -> findMaximum (n-1) str
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

findMaximumWithin _ 0 curMax str = curMax*10 + maximum str
findMaximumWithin n size curMax str = case elemIndex n str of
  Just idxN -> if (length str - idxN) > size then findMaximumWithin 9 (size-1) (curMax*10 + n) (drop (idxN+1) str)
                                           else findMaximumWithin (n-1) size curMax str
  Nothing -> findMaximumWithin (n-1) size curMax str
