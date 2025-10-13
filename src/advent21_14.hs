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
import qualified MyUtils as MU

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
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
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)

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
  putStrLn "** Advent 2021 - Day 14 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let polymerStart = head (lines content)
  let ruleM = Map.fromList $ map parseRule (drop 2 (lines content))
  -- putStrLn $ "ruleM=" ++ show ruleM

  let freqInit = startToFreq Map.empty polymerStart
  -- putStrLn $ "freqInit=" ++ show freqInit
  let freq10 = MU.applyN (Map.foldrWithKey (opPolymerize ruleM) Map.empty) 10 freqInit
  -- putStrLn $ "freqN=" ++ show freqN

  let count10 = Map.foldrWithKey opCount Map.empty freq10
  let finalCount10 = Map.toList $ Map.insertWith (+) (head polymerStart) 1 count10
  -- putStrLn $ "count=" ++ show finalCount
  let pRes = scorePolymer finalCount10
  putStrLn $ "Answer 1> " ++ show pRes

  let freq40 = MU.applyN (Map.foldrWithKey (opPolymerize ruleM) Map.empty) 40 freqInit
  -- putStrLn $ "freqN=" ++ show freqN

  let count40 = Map.foldrWithKey opCount Map.empty freq40
  let finalCount40 = Map.toList $ Map.insertWith (+) (head polymerStart) 1 count40
  -- putStrLn $ "count=" ++ show finalCount
  let cRes = scorePolymer finalCount40
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
-- the idea is to count XY -> nb, add the total number of letters is counted on
-- the second char (Y) of th XY pattern
type RuleMap = Map.Map String String
type FreqMap = Map.Map String Int
type CountMap = Map.Map Char Int

parseRule :: String -> (String, String)
parseRule line = (pat, ch)
  where
    (pat:_:ch:_) = words line

startToFreq :: FreqMap -> String -> FreqMap
startToFreq _ [] = error "startToFreq with empty list"
startToFreq freqM [_] = freqM
startToFreq freqM (c0:c1:cs) = startToFreq newFreqM (c1:cs)
  where
    newFreqM = Map.insertWith (+) [c0,c1] 1 freqM

-- applyRules used on every pattern of FreqMap to create newFreqMap
opPolymerize :: RuleMap -> String -> Int -> FreqMap -> FreqMap
opPolymerize ruleM pat nb newFreqM = rightFM
  where
    (cl:cr:_) = pat
    insertedChar = ruleM Map.! pat
    leftFM = Map.insertWith (+) (cl:insertedChar) nb newFreqM
    rightFM = Map.insertWith (+) [head insertedChar,cr] nb leftFM

opCount :: String -> Int -> CountMap -> CountMap
opCount pat nb countM = newCountM
  where
    (_:cr:_) = pat
    newCountM = Map.insertWith (+) cr nb countM

scorePolymer :: [(Char, Int)] -> Int
scorePolymer counts = maximum freq - minimum freq
  where
    freq = map snd counts
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

