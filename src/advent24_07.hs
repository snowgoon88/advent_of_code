{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
-- import Data.List ( sort, group, sortOn, groupBy ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
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
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace

l01 = "3267: 81 40 27"
e01 = parseLine l01

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 07 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input07.txt"
  -- content <- readFile "Input24/test07_1.txt"

  let allEq = map parseLine (lines content)
  let solvable = filter (\e -> eval (eqres e) (eqnbs e)) allEq

  let pRes = sum (map eqres solvable)
  putStrLn $ "Answer 1> " ++ show pRes

  let solvableAdv = filter (\e -> evalAdv (eqres e) (eqnbs e)) allEq

  let cRes = sum (map eqres solvableAdv)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data OP = OpM | OpP | OpC deriving Show

data Equation = EQU { eqres :: Int
                   , eqnbs :: [Int] }
  deriving Show

parseLine line = EQU res allNb
  where
    tokens = split ":" line
    res = read (head tokens)
    nbTok = split " " (tokens !! 1)
    allNb = map read (drop 1 nbTok)


applyOp :: Int -> Int -> Int -> OP -> (Int, Bool)
applyOp res n1 n2 OpM = (n1 * n2, (n1 * n2) <= res)
applyOp res n1 n2 OpP = (n1 + n2, (n1 + n2) <= res)
applyOp res n1 n2 OpC = (nOp, nOp <= res)
  where
    nOp = read (show n1 ++ show n2)

eval :: Int -> [Int] -> Bool
eval res [nb] = res == nb
eval res (n1:n2:ns) = op1 || op2
  where
    op1 = case applyOp res n1 n2 OpM of
      (r1, True) -> eval res (r1:ns)
      (_, False) -> False
    op2 = case applyOp res n1 n2 OpP of
      (r2, True) -> eval res (r2:ns)
      (_, False) -> False

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

evalAdv :: Int -> [Int] -> Bool
evalAdv res [nb] = res == nb
evalAdv res (n1:n2:ns) = op1 || op2 || op3
  where
    op1 = case applyOp res n1 n2 OpM of
      (r1, True) -> evalAdv res (r1:ns)
      (_, False) -> False
    op2 = case applyOp res n1 n2 OpP of
      (r2, True) -> evalAdv res (r2:ns)
      (_, False) -> False
    op3 = case applyOp res n1 n2 OpC of
      (r3, True) -> evalAdv res (r3:ns)
      (_, False) -> False
