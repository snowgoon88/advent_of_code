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
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
import Data.Maybe ( catMaybes )
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

import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PP
import Data.Void (Void)

-- *********************************************************************************** DEBUG
import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
debug = flip trace


exp1 :: String
exp1 = "1 + 2 * 3 + 4 * 5 + 6"
exp2 :: String
exp2 = "1 + (2 * 3) + (4 * (5 + 6))"
exp3 :: String
exp3 = "2 * 3 + (4 * 5)"
exp4 :: String
exp4 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"
exp5 :: String
exp5 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
exp6 :: String
exp6 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
exp7 :: String
exp7 = "2 * (2 * 3 + 1) + 4"
exp8 :: String
exp8 = "(3 * (3 + 3) * 9 + (6 + 9 * 8) + 5) + 5"

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 18 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input18.txt"
  -- content <- readFile "Input20/test18_1.txt"

  let allExp = map (\l -> fst ( evalExp (0, '+', l))) (lines content)
  let pRes = sum allExp
  putStrLn $ "Answer 1> " ++ show pRes

  -- let allExpAdv = map (\l -> fst ( evalExpAdv (0, '+', l))) (lines content)
  -- let cRes = sum allExpAdv
  -- putStrLn $ "Answer 2> " ++ show cRes

  let allSmart = map (P.parseMaybe parseTop2) (lines content)
  let cRes = sum (catMaybes allSmart)
  putStrLn $ "Answer 2> " ++ show cRes
  -- putStrLn (unlines res)

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
evalExp (acc, _, []) = (acc, [])
evalExp (acc, _, ')':xs) = (acc, xs) --`debug` niceStr (acc, "X", ")", xs)
evalExp (acc, op, ' ':xs) = evalExp (acc, op, xs) --`debug` niceStr (acc, show op, " ", xs)
evalExp (acc, '+', '(':xs) = evalExp (acc + res, '+', toParse) --`debug` niceStr (acc, "+", "(", xs)
  where
    (res, toParse) = evalExp (0, '+', xs)
evalExp (acc, '*', '(':xs) = evalExp (acc * res, '+', toParse) --`debug` niceStr (acc, "*", "(", xs)
  where
    (res, toParse) = evalExp (0, '+', xs)
evalExp (acc, _, '+':xs) = evalExp (acc, '+', xs)
evalExp (acc, _, '*':xs) = evalExp (acc, '*', xs)
evalExp (acc, '+', x:xs) = evalExp (acc + digitToInt x, '+', xs)
evalExp (acc, '*', x:xs) = evalExp (acc * digitToInt x, '*', xs)

niceStr (acc, op, x, xs) = "acc=" ++ show acc ++ "[" ++ op ++ "] " ++ x ++ ":" ++ xs
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
evalExpAdv (acc, _, []) = (acc, [])
evalExpAdv (acc, _, ')':xs) = (acc, xs) `debug` niceStr (acc, "X", ")", xs)
evalExpAdv (acc, op, ' ':xs) = evalExpAdv (acc, op, xs) -- `debug` niceStr (acc, show op, " ", xs)
evalExpAdv (acc, '+', '(':xs) = evalExpAdv (acc + res, '+', toParse) `debug` niceStr (acc + res, "+", "(", xs)
  where
    (res, toParse) = evalExpAdv (0, '+', xs)
evalExpAdv (acc, '*', '(':xs) = evalExpAdv (acc * res, '+', toParse) `debug` niceStr (acc * res, "*", "(", xs)
  where
    (res, toParse) = evalExpAdv (0, '+', xs)
evalExpAdv (acc, _, '+':xs) = evalExpAdv (acc, '+', xs) `debug` niceStr (acc, "X", "+", xs)
evalExpAdv (acc, _, '*':xs) = evalExpAdv (acc, '*', xs) `debug` niceStr (acc, "X", "*", xs)
evalExpAdv (acc, '+', x:xs) = evalExpAdv (acc + digitToInt x, '+', xs) `debug` niceStr (acc, "+", show x, xs)
-- '*' is has a lower priority
evalExpAdv (acc, '*', x:xs) = evalExpAdv (acc * res, '*', toParse) `debug` niceStr (acc, "*", show res, toParse)
  where
    (res, toParse) = evalExpAdv (digitToInt x, '+', xs)

-- *****************************************************************************
-- *********************************************************************** SMART
-- *****************************************************************************
type Parser = P.Parsec Void String

parseBottom1 :: Parser Int
parseBottom1 = P.choice
    [ PP.decimal
    , P.between "(" ")" parseTop1  -- use -XOverloadedStrings to get parsers that match strings
    ]

parseTop1 :: Parser Int
parseTop1 = do
    leftOfOp <- parseBottom1   -- parse the left hand side of a possible binary operator
    doNext leftOfOp
  where
    doNext acc = P.choice          -- once we parse a left hand side, pick from:
      [ do " * "                        -- either it's a *
           rightOfOp <- parseBottom1    --   ... so we parse the right hand side and multiply
           doNext (acc * rightOfOp)
      , do " + "                        -- or it's a +
           rightOfOp <- parseBottom1    --   ... so we parse the right hand side and add
           doNext (acc + rightOfOp)
      , pure acc                        -- otherwise that was it, no operator
      ]

parseBottom2 :: Parser Int
parseBottom2 = P.choice
    [ PP.decimal
    , P.between "(" ")" parseTop2
    ]

parseMiddle2 :: Parser Int
parseMiddle2 = do
    leftOfOp <- parseBottom2
    doNext leftOfOp
  where
    doNext acc = P.choice
      [ do " + "
           rightOfOp <- parseBottom2
           doNext (acc + rightOfOp)
      , pure acc
      ]

parseTop2 :: Parser Int
parseTop2 = do
    leftOfOp <- parseMiddle2
    doNext leftOfOp
  where
    doNext acc = P.choice
      [ do " * "
           rightOfOp <- parseMiddle2
           doNext (acc * rightOfOp)
      , pure acc
      ]


compareParse :: String -> String
compareParse input = if valSmart /= valMe
  then input ++ "\n" ++ "valSmart = " ++ show valSmart ++ "\n" ++ "valMe = " ++ show valMe
  else "\n"
  where
    Just valSmart = P.parseMaybe parseTop2 input
    (valMe, _) = evalExpAdv (0, '+', input)
