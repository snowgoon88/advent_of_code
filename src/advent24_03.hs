{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import Data.List ( sort, group ) --sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State

l01 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
l02 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 03 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input03.txt"
  -- content <- readFile "Input24/test03_1.txt"

  let ops = parseLine content
  -- let ops = parseLine l01
  -- print $ "ops=" ++ show ops

  let pRes = sum (map opMul ops)
  putStrLn $ "Answer 1> " ++ show pRes

  -- let opsE01 = parseMul True [] l01
  -- print $ "opsE=" ++ show opsE01
  -- let cRes01 = sum (map opMul opsE01)
  -- putStrLn $ "Answer 2> " ++ show cRes01

  -- let opsE02 = parseMul True [] l02
  -- print $ "opsE=" ++ show opsE02
  -- let cRes02 = sum (map opMul opsE02)
  -- putStrLn $ "Answer 2> " ++ show cRes02

  let opsE = parseMul True [] content
  -- print $ "opsE=" ++ show opsE
  let cRes = sum (map opMul opsE)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

parseLine line = map (parseFirstArg "") tokens
  where
    tokens = split "mul(" line

parseFirstArg :: String -> String -> (Maybe Int, Maybe Int)
parseFirstArg acc (c:cs)
  | isDigit c = parseFirstArg (acc ++ [c]) cs
  | c == ',' = (Just (read acc), parseSecondArg "" cs)
  | otherwise = (Nothing, Nothing)

parseSecondArg :: String -> String -> Maybe Int
parseSecondArg acc (c:cs)
  | isDigit c = parseSecondArg (acc ++ [c]) cs
  | c == ')' = Just (read acc)
  | otherwise = Nothing

opMul :: (Maybe Int, Maybe Int) -> Int
opMul (Just a, Just b) = a*b
opMul _ = 0

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

parseMul :: Bool -> [(Maybe Int, Maybe Int)] -> String -> [(Maybe Int, Maybe Int)]
parseMul True lOps  ('d':'o':'n':'\'':'t':'(':')':cs) = parseMul False lOps cs
parseMul True lOps  ('m':'u':'l':'(':cs) = parseFirstArgEnabled lOps "" cs
parseMul True lOps  (c:cs) = parseMul True lOps cs
parseMul False lOps ('d':'o':'(':')':cs) = parseMul True lOps cs
parseMul False lOps (c:cs) = parseMul False lOps cs
parseMul _ lOps [] = lOps

parseFirstArgEnabled :: [(Maybe Int, Maybe Int)] -> String -> String -> [(Maybe Int, Maybe Int)]
parseFirstArgEnabled lOps acc (c:cs)
  | isDigit c = parseFirstArgEnabled lOps (acc ++ [c]) cs
  | c == ','  = parseSecondArgEnabled lOps (read acc) "" cs
  | otherwise = parseMul True ((Nothing, Nothing):lOps) (c:cs)

parseSecondArgEnabled :: [(Maybe Int, Maybe Int)] -> Int -> String -> String -> [(Maybe Int, Maybe Int)]
parseSecondArgEnabled lOps firstArg acc (c:cs)
  | isDigit c = parseSecondArgEnabled lOps firstArg (acc ++ [c]) cs
  | c == ')' = parseMul True ((Just firstArg, Just (read acc)):lOps) cs
  | otherwise = parseMul True ((Nothing, Nothing):lOps) (c:cs)
