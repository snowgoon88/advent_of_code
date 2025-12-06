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
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.Foldable ( foldl' )
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
import GHC.IO.Exception (IOErrorType(UnsupportedOperation))

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
  putStrLn "** Advent 2025 - Day 06 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let parsed = words content
  -- putStrLn $ "parsed=" ++ show parsed

  let pbs = initPb [] (reverse parsed)
  -- putStrLn $ "pbs=" ++ show pbs
  let solved = solvePb (reverse (fst pbs)) (snd pbs)
  -- putStrLn $ "solved=" ++ show solved
  let pRes = sum $ map getVal solved
  putStrLn $ "Answer 1> " ++ show pRes

  let lMap = parseLineMap Map.empty 0 content
  -- putStrLn $ "lMap=" ++ show lMap
  putStrLn $ "size=" ++ show (Map.size lMap)
  let idxOp = dropWhile (\(k, v) -> v /= '+' && v /= '*') $ Map.toAscList lMap
  -- putStrLn $ "idxOp=" ++ show idxOp
  let nbOp = Map.size $ Map.filter (\v -> v =='+' || v == '*') lMap
  let posOp = Map.keys $ Map.filter (\v -> v =='+' || v == '*') lMap
  -- putStrLn $ "posOp=" ++ show posOp
  let idxFstOp = head posOp
  let idxLastOp = head (reverse posOp)
  putStrLn $ "pos in " ++ show idxFstOp ++ " - " ++ show idxLastOp
  let nbRow = divMod idxFstOp (idxLastOp - idxFstOp)
  let sizeRow = divMod idxFstOp (fst nbRow)
  putStrLn $ "nbRow=" ++ show nbRow ++ " => " ++ "row of size " ++ show sizeRow

  let gMap = remap2D (Map.filterWithKey (\k _ -> k < idxFstOp) lMap) (fst sizeRow)
  -- putStrLn $ "gMap=" ++ show gMap
  let pMap = Map.toList $ Map.filter (\c -> c == '+' || c == '*') lMap
  -- putStrLn $ "nb0=" ++ show (intAtColN gMap 0)
  -- putStrLn $ "nb1=" ++ show (intAtColN gMap 1)
  -- putStrLn $ "nb2=" ++ show (intAtColN gMap 2)
  let colPb = map (\k -> k - idxFstOp) posOp
  putStrLn $ "solPb01=" ++ show (solveCol gMap idxFstOp (fst sizeRow) pMap )

  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Problem = Plus Int | Times Int
  deriving ( Show )
getVal (Plus x) = x
getVal (Times x) = x

initPb pbList ("+":xs) = initPb (Plus 0:pbList) xs
initPb pbList ("*":xs) = initPb (Times 1:pbList) xs
initPb pbList xs = (pbList, xs)

stepPb (Plus acc, str) = Plus (acc + read str)
stepPb (Times acc, str) = Times (acc * read str)

solvePb pbs [] = pbs
solvePb pbs nbs = solvePb stepped toStep
  where
    stepped = map stepPb (zip pbs nbs)
    toStep = drop (length pbs) nbs

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
type LineMap = Map.Map Int Char
type GridMap = Map.Map (Int, Int) Char

parseLineMap lMap _ [] = lMap
parseLineMap lMap acc ('\n':cs) = parseLineMap lMap acc cs
parseLineMap lMap acc (c:cs) = parseLineMap (Map.insert acc c lMap) (acc+1) cs

remap2D lMap rowSize = Map.mapKeys (\k -> divMod k rowSize) lMap

intAtColN :: GridMap -> Int -> Int
intAtColN gMap col = read $ Map.elems $ Map.filterWithKey (\(r,c) _ -> c == col) gMap


solveCol gMap idxFst rowSize [(p1,t1)]
  -- | t1 == '+' = [('+', nbs)]
  -- | t1 == '+' = [('+', nbs)]
  | t1 == '+' = sum nbs
  | t1 == '*' = product nbs
  | otherwise = error ("op not recognised:" ++ [t1] ++ "-")
  where
    nbs = map (intAtColN gMap) [(p1-idxFst) .. (rowSize-1)]
solveCol gMap idxFst rowSize ((p1,t1):(p2,t2):ps)
  -- | t1 == '+' = ('+', nbs) : solveCol gMap idxFst rowSize pbOpen
  -- | t1 == '+' = ('+', nbs) : solveCol gMap idxFst rowSize pbOpen
  | t1 == '+' = sum nbs + solveCol gMap idxFst rowSize pbOpen
  | t1 == '*' = product nbs + solveCol gMap idxFst rowSize pbOpen
  | otherwise = error ("op not recognised:" ++ [t1] ++ "-")
  where
    nbs = map (intAtColN gMap) [(p1-idxFst) .. (p2-2-idxFst)]
    pbOpen = (p2,t2):ps
