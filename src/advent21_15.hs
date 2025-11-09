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
import qualified MyGrid as MG
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines
import qualified MySearch as MS

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
import Data.Char ( digitToInt )
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
  putStrLn "** Advent 2021 - Day xx Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let riskMap = parseRisk (lines content)
  putStrLn $ "size=" ++ show (fst riskMap)
  -- putStrLn $ "risk=" ++ show (snd riskMap)

  -- putStrLn $ "testGoal 3 3 = " ++ show (isEndPosition riskMap (3, 3))
  -- putStrLn $ "testGoal 9 9 = " ++ show (isEndPosition riskMap (9, 9))
  -- putStrLn $ "testhDist 3 3 = " ++ show (hDist riskMap (3, 3))
  -- putStrLn $ "testhDist 9 9 = " ++ show (hDist riskMap (9, 9))
  -- putStrLn $ "testNeigh 3 3 = " ++ show (getNeighbors riskMap (3, 3))
  -- putStrLn $ "testNeigh 0 3 = " ++ show (getNeighbors riskMap (0, 3))

  -- putStrLn $ "bestPath=" ++ show ( MS.aStar (isEndPosition riskMap)
  --                                    (hDist riskMap)
  --                                    (getNeighbors riskMap)
  --                                    (0, 0))

  let pRes = bestPath riskMap
  putStrLn $ "Answer 1> " ++ show pRes

  -- putStrLn $ "testNV 1 1 =" ++ show (newRisk riskMap (1, 1))
  -- putStrLn $ "testNV 11 41 =" ++ show (newRisk riskMap (31, 41))
  -- putStrLn $ "testNV 0 0 =" ++ show (newRisk riskMap (0, 0))
  -- putStrLn $ "testNV 0 9 =" ++ show (newRisk riskMap (0, 9))
  -- putStrLn $ "testNV 1 9 =" ++ show (newRisk riskMap (1, 9))
  -- putStrLn $ "testNV 2 9 =" ++ show (newRisk riskMap (2, 9))
  -- putStrLn $ "testNV 3 9 =" ++ show (newRisk riskMap (3, 9))
  -- putStrLn $ "testNV 3 19 =" ++ show (newRisk riskMap (3, 19))
  -- putStrLn $ "testNV 3 29 =" ++ show (newRisk riskMap (3, 29))
  -- putStrLn $ "testNV 3 49 =" ++ show (newRisk riskMap (3, 49))
  -- putStrLn $ "testNV 3 50 =" ++ show (newRisk riskMap (3, 50))

  let cRes = newPath riskMap
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type RiskMap = MG.GridMapCore Int

parseRisk :: [String] -> RiskMap
parseRisk ls = (s, Map.map digitToInt charMap)
  where
    (s, charMap) = MG.readGrid ls

{- In order to use aStar, we need:
   - State/Node of aStar: MG.Pos
   - isGoal: Pos -> Pool
   - hDist: Pos -> Float
   - getNeighbors: Pos -> [(Float, Pos)]
-}
isEndPosition :: RiskMap -> MG.Pos -> Bool
isEndPosition ((sizeRows, sizeCols), _) (row, col) = (row == sizeRows - 1) && (col == sizeCols - 1)
hDist :: RiskMap -> MG.Pos -> Float
hDist ((sizeRows, sizeCols), _) (row, col) = fromIntegral (sizeRows - row + sizeCols - col)
getNeighbors :: RiskMap -> MG.Pos -> [(Float, MG.Pos)]
getNeighbors (_, riskMap) (row, col) = distNeighbors
  where
    posNeighbors = map (MG.addDir (row, col)) [(1, 0), (0, 1), (-1, 0), (0, -1)]
    distNeighbors = foldr opNeigh [] posNeighbors
    opNeigh p neighbors = case Map.lookup p riskMap of
      Just cost -> (fromIntegral cost, p):neighbors
      Nothing   -> neighbors

bestPath :: RiskMap -> Float
bestPath ((sr, sc), rMap) = case sol of
  Just (_, aStarMemory) -> MS.gScore (aStarMemory Map.! (sr-1, sc-1))
  Nothing -> -1.0
  where
    sol = MS.aStar (isEndPosition ((sr, sc), rMap))
                   (hDist ((sr, sc), rMap))
                   (getNeighbors ((sr, sc), rMap))
                   (0, 0)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- here, risk from original riskMap and Pos
newRisk :: RiskMap -> MG.Pos -> Maybe Int
newRisk ((sr, sc), riskM) (r, c)
  | nr >= 0 && nr < 5 && nc >= 0 && nc < 5 = Just newVal
  | otherwise = Nothing
  where
    (nr, rr) = divMod r sr
    (nc, rc) = divMod c sc
    v = riskM Map.! (rr, rc)
    newVal = 1 + mod (v - 1 + nr + nc) 9

newGoal :: RiskMap -> MG.Pos -> Bool
newGoal ((sizeRows, sizeCols), _) (row, col) = (row == 5*sizeRows - 1) && (col == 5*sizeCols - 1)
newDist :: RiskMap -> MG.Pos -> Float
newDist ((sizeRows, sizeCols), _) (row, col) = fromIntegral (5*sizeRows - row - 1 + 5*sizeCols - 1 - col)
newNeigh :: RiskMap -> MG.Pos -> [(Float, MG.Pos)]
newNeigh (s, riskMap) (row, col) = distNeighbors
  where
    posNeighbors = map (MG.addDir (row, col)) [(1, 0), (0, 1), (-1, 0), (0, -1)]
    distNeighbors = foldr opNeigh [] posNeighbors
    opNeigh p neighbors = case newRisk (s, riskMap) p of
      Just cost -> (fromIntegral cost, p):neighbors
      Nothing   -> neighbors

newPath :: RiskMap -> Float
newPath ((sr, sc), rMap) = case sol of
  Just (_, aStarMemory) -> MS.gScore (aStarMemory Map.! (5*sr-1, 5*sc-1))
  Nothing -> -1.0
  where
    sol = MS.aStar (newGoal ((sr, sc), rMap))
                   (newDist ((sr, sc), rMap))
                   (newNeigh ((sr, sc), rMap))
                   (0, 0)
