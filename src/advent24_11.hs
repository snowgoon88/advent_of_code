{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
import Data.List ( sort, groupBy, group ) -- sortOn, groupBy, find, group, sort, sortBy )
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

s01 = "125 17"

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 11 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input11.txt"
  -- content <- readFile "Input24/test11_1.txt"

  -- let stonesInit = map read (split " " s01) :: [Int]
  -- print $ "stonesInit=" ++ show stonesInit
  let stonesInit = map read (split " " content) :: [Int]
  -- let step01 = blink stonesInit
  -- print $ "step01=" ++ show step01

  let stonesFinal = transform stonesInit 25
  -- print $ "stonesFinal=" ++ show stonesFinal

  let pRes = length (stonesFinal)
  putStrLn $ "Answer 1> " ++ show pRes

  -- let freqInit = parseFreq s01
  let freqInit = parseFreq content

  let freqFinal = transformFreq freqInit 75
  -- print $ "freqFinal=" ++ show freqFinal

  let cRes = sum (map snd freqFinal)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

transform :: [Int] -> Int -> [Int]
transform stones nb = foldl (\s _ -> blink s) stones [1 .. nb]

blink :: [Int] -> [Int]
blink [] = []
blink (stone:ss) = rule stone ++ blink ss

rule :: Int -> [Int]
rule stone
  | stone == 0 = [1]
  | even stoneLen = [read (take (div stoneLen 2) stoneStr), read (drop (div stoneLen 2) stoneStr)]
  | otherwise = [2024 * stone]
    where
      stoneStr = show stone
      stoneLen = length stoneStr

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

{-
  even : 2n => goes to n * 1 en n steps
  odd * 2024 = odd * 1012 * 2 = decompose
  0 -> 1
  1 -> 2024 => 20 24 => 2 0 2 4
  2 -> 4048 => 40 48 => 4 0 4 8
  3 -> 6072 => 60 72 => 6 0 7 2
  4 -> 8096 => 80 96 => 8 0 9 6
  5 -> odd 10120 => 20482880 => 2048 2880 => 20 48 28 80 => 2 0 4 8 2 8 8 0
  6 -> odd 12144 => 24579456 => 2457 9456 => 24 57 94 56 => 2 4 5 7 9 4 5 6
  7 -> odd 14168
  8 -> odd 16192
  9 -> odd 18216 => 3 6 8 6 9 1 8 4

  nb 0 = (1-nb) 1 =
-}

-- (nb, times)
type Freq = (Int, Int)

parseFreq :: String -> [Freq]
parseFreq line = map (\s -> (read s, 1)) tok
  where tok = split " " line

transformFreq :: [Freq] -> Int -> [Freq]
transformFreq freqs nb = foldl (\s _ -> blinkFreq s) freqs [1 .. nb]

blinkFreq :: [Freq] -> [Freq]
blinkFreq freqs = gather $ concat $ map ruleFreq freqs

ruleFreq :: Freq -> [Freq]
ruleFreq (stone, freq)
  | stone == 0 = [(1, freq)]
  | even stoneLen = [(read (take (div stoneLen 2) stoneStr), freq),
                     (read (drop (div stoneLen 2) stoneStr), freq)]
  | otherwise = [(2024 * stone, freq)]
    where
      stoneStr = show stone
      stoneLen = length stoneStr

gather :: [Freq] -> [Freq]
gather freqs = map sumFreq gfreq
  where
    gfreq = groupBy (\f1 f2 -> (fst f1) == (fst f2)) $ sort freqs
    sumFreq :: [Freq] -> Freq
    sumFreq lf = (fst $ head lf, sum (map snd lf))
