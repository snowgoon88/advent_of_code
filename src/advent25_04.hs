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
import qualified MyUtils as MU

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
import Data.List ( delete )
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
  putStrLn "** Advent 2021 - Day 04 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let grid = MG.readGrid (lines content)
  putStrLn $ "read grid of size=" ++ show (fst grid)
  -- let pRes = Map.foldrWithKey (opFree (snd grid)) (0, []) (snd grid)
  -- putStrLn $ "Answer 1> " ++ show pRes

  -- let (g01, s01, d01) = stepRemove (snd grid, 0, 0)
  -- putStrLn $ MG.showGrid (fst grid, g01)
  -- putStrLn $ "nb=" ++ show s01 ++ " (" ++ show d01 ++ ")"
  -- let (g02, s02, d02) = stepRemove (g01, s01, d01)
  -- putStrLn $ MG.showGrid (fst grid, g02)
  -- putStrLn $ "nb=" ++ show s02 ++ " (" ++ show d02 ++ ")"

  let neighbors = toNeighborMap (snd grid)
  putStrLn $ "size0=" ++ show (Map.size neighbors)
  let (n01, a01, d01) = stepNeighbors (neighbors, 0, -1)
  putStrLn $ "size1=" ++ show (Map.size n01)
  putStrLn $ "nb=" ++ show a01


  -- let (gf, sf, df) = until (\(_, _, d) -> d == 0) stepRemove (snd grid, 0, -1)
  -- -- putStrLn $ MG.showGrid (fst grid, gf)
  -- putStrLn $ "nb=" ++ show sf ++ " (" ++ show df ++ ")"
  let (nf, sf, df) = until (\(_, _, d) -> d == 0) stepNeighbors (neighbors, 0, -1)
  -- putStrLn $ MG.showGrid (fst grid, gf)
  putStrLn $ "nb=" ++ show sf ++ " (" ++ show df ++ ")"

  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type GridMap = MG.PosMapCore Char

getSurround :: GridMap -> MG.Pos -> [Char]
getSurround gMap key = Map.elems $ Map.filterWithKey (\k _ -> k `elem` neighbors) gMap
  where
    neighbors = map (MG.addDir key) MG.allDir

getFreeRoll :: GridMap -> MG.Pos -> Int
getFreeRoll gMap key = MU.countTrue (=='@') (getSurround gMap key)

opFree :: GridMap -> MG.Pos -> Char -> (Int, [(MG.Pos, Int)]) -> (Int, [(MG.Pos, Int)])
opFree gMap key item (acc, ks) = if item == '@' && count < 4 then (acc + 1, (key, count):ks) else (acc, ks)
  where count = getFreeRoll gMap key
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

listFree :: GridMap -> MG.Pos -> Char -> [MG.Pos] -> [MG.Pos]
listFree gMap key item ks = if item == '@' && count < 4 then key:ks else ks
  where count = getFreeRoll gMap key

stepRemove (gMap, acc, _) = (newMap, acc+length lk, length lk)
  where
    lk = Map.foldrWithKey (listFree gMap) [] gMap
    newMap = foldr (`Map.insert` '.') gMap lk


-- More SMART: Map Pos -> [Neighbors]
type NeighborsMap = MG.PosMapCore [MG.Pos]

toNeighborMap :: GridMap -> NeighborsMap
toNeighborMap gMap = Map.mapWithKey (getSurroundRolls gMap) (Map.filter (=='@') gMap)

getSurroundRolls :: GridMap -> MG.Pos -> Char -> [MG.Pos]
getSurroundRolls gMap key _ = Map.keys $ Map.filterWithKey (\k c -> c == '@' && k `elem` neighbors) gMap
  where
    neighbors = map (MG.addDir key) MG.allDir

-- remove 'posRemoved' from the neighbors of 'posNeighbors'
opRemoveNeighbor :: MG.Pos -> MG.Pos -> NeighborsMap -> NeighborsMap
opRemoveNeighbor posRemoved posNeighbor nMap = Map.adjust (delete posRemoved) posNeighbor nMap

opRemoveRoll :: NeighborsMap -> MG.Pos -> NeighborsMap -> NeighborsMap
opRemoveRoll oriNMap pos nMap = foldr (opRemoveNeighbor pos) purgedMap nPos
  where
    nPos = oriNMap Map.! pos
    purgedMap = Map.delete pos nMap

stepNeighbors (nMap, acc, diff) = (newMap, acc + length posToRemove, length posToRemove)
  where
    posToRemove = Map.keys $ Map.filter (\ln -> length ln < 4) nMap
    newMap = foldr (opRemoveRoll nMap) nMap  posToRemove
