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
import Data.Char (digitToInt)
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
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
import Debug.Trace ( trace ) -- trace :: String > a -> a
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2021 - Day 03 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input21/input03.txt"
  -- content <- readFile "Input21/test03_1.txt"

  let counts = countOnes (lines content)
  putStrLn $ "counts=" ++ show counts
  let gammaB = keepMaj (counts)
  let epsilonB = complement gammaB
  let pRes = binToDecimal gammaB * binToDecimal epsilonB
  putStrLn $ "Answer 1> " ++ show pRes

  let allBits = map parseLine (lines content)
  let oxygen = buildOxygen allBits
  putStrLn $ "oxygen=" ++ show oxygen ++ " => " ++ show (binToDecimal oxygen)
  let co2scruber = buildCOtwo allBits
  putStrLn $ "co2scruber=" ++ show co2scruber++ " => " ++ show (binToDecimal co2scruber)
  let cRes = binToDecimal oxygen * binToDecimal co2scruber
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
parseLine :: String -> [Int]
parseLine line = map digitToInt line

-- count nb of lines, and nb of "Ones" at each pos
countOnes :: [String] -> (Int, [Int])
countOnes ls = foldr opCount (0, repeat 0) ls
  where
    opCount line (acc, counts) = (acc+1,
                                  zipWith (+) counts (parseLine line))

keepMaj :: (Int, [Int]) -> [Int]
keepMaj (acc, counts) = map (\d -> if d > div acc 2 then 1 else 0) counts

complement :: [Int] -> [Int]
complement counts = map (\d -> 1 - d) counts

binToDecimal :: [Int] -> Int
binToDecimal bs = foldl (\c b -> c * 2 + b) 0 bs
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

buildOxygen :: [[Int]] -> [Int]
buildOxygen [] = []
buildOxygen [unique] = unique
buildOxygen bs = if majOne then 1 : buildOxygen withOne  -- `debug` ("1 - " ++ show withOne ++ "\n")
                              else 0 : buildOxygen withZero -- `debug` ("0 - " ++ show withZero ++ "\n")
  where
    majOne = ones >= (nb-ones) -- `debug` ("maj=" ++ show ones ++ "/" ++ show (div nb 2) ++ "\n")
    (nb, ones, withOne, withZero) = countOnesStart bs

buildCOtwo :: [[Int]] -> [Int]
buildCOtwo [] = []
buildCOtwo [unique] = unique
buildCOtwo bs = if majOne then 0 : buildCOtwo withZero  -- `debug` ("1 - " ++ show withOne ++ "\n")
                          else 1 : buildCOtwo withOne -- `debug` ("0 - " ++ show withZero ++ "\n")
  where
    majOne = ones >= (nb-ones) -- `debug` ("maj=" ++ show ones ++ "/" ++ show (div nb 2) ++ "\n")
    (nb, ones, withOne, withZero) = countOnesStart bs

countOnesStart :: [[Int]] -> (Int, Int, [[Int]], [[Int]])
countOnesStart bs = foldr opCount (0, 0, [], []) bs
  where
    opCount (1:cs) (acc, counts, wO, wZ) = (acc+1, counts+1, cs:wO, wZ)
    opCount (0:cs) (acc, counts, wO, wZ) = (acc+1, counts, wO, cs:wZ)
