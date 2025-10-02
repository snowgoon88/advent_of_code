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

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
import Data.Char (digitToInt)
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
import Data.Maybe (mapMaybe)
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List (sort)
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
  putStrLn "** Advent 2021 - Day 09 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let heightMap = readHeightMap (lines content)
  let valMinimae = localMinimae heightMap
  -- putStrLn $ "valMini=" ++ show valMinimae

  let pRes = sum (map (+1) valMinimae)
  putStrLn $ "Answer 1> " ++ show pRes

  let posMin = posMinimae heightMap
  -- putStrLn $ "posMin=" ++ show posMin

  -- let size01 = recBassin heightMap Set.empty (Set.fromList [(0, 1)])
  -- putStrLn $ "size01=" ++ show size01
  let sizes = map (\p -> recBassin heightMap Set.empty (Set.fromList [p])) posMin
  -- putStrLn $ "sizes=" ++ show sizes
  let cRes = product (take 3 $ (reverse . sort) sizes)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type HeightMap = MG.PosMapCore Int

readHeightMap :: [String] -> HeightMap
readHeightMap ls = Map.map digitToInt charMap
  where
    (_, charMap) = MG.readGrid ls

neighbor4 :: [MG.DirVec]
neighbor4 = [(1, 0), (0, 1), (-1, 0), (0, -1)]


localMinimae :: HeightMap -> [Int]
localMinimae hMap = foldr (opMinHeight hMap) [] (Map.keys hMap)

-- add to `acc` if local minima
opMinHeight :: HeightMap -> MG.Pos -> [Int] -> [Int]
opMinHeight hMap p acc = if localHeight < miniNeigh then localHeight:acc
                                                    else acc
  where
    localHeight = hMap Map.! p
    -- for all NeighborsVec, create Pos, then ask val in Map and take minimum
    miniNeigh = minimum $ mapMaybe ((hMap Map.!?) . MG.addDir p) neighbor4

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

posMinimae :: HeightMap -> [MG.Pos]
posMinimae hMap = foldr (opPosMinHeight hMap) [] (Map.keys hMap)

-- add to `acc` if local minima
opPosMinHeight :: HeightMap -> MG.Pos -> [MG.Pos] -> [MG.Pos]
opPosMinHeight hMap p acc = if localHeight < miniNeigh then p:acc
                                                    else acc
  where
    localHeight = hMap Map.! p
    -- for all NeighborsVec, create Pos, then ask val in Map and take minimum
    miniNeigh = minimum $ mapMaybe ((hMap Map.!?) . MG.addDir p) neighbor4

-- for a bassin, expand from local min into a set of position if not 9 and
-- if not already tested
recBassin :: HeightMap -> Set.Set MG.Pos -> Set.Set MG.Pos -> Int
recBassin hMap seenSet pSet
  | Set.null pSet = 0
  | otherwise = if localHeight < 9 then 1 + recBassin hMap newSeen newPSet
                                   else recBassin hMap newSeen toTest       -- no new pos to test
  where
    (posSingleton, toTest) = Set.splitAt 1 pSet
    pos = Set.elemAt 0 posSingleton
    newSeen = Set.insert pos seenSet
    localHeight = hMap Map.! pos
    newPSet = Set.difference (Set.union toTest (Set.fromList neighbors)) newSeen
    neighbors = filter (`Map.member` hMap) (map (MG.addDir pos) neighbor4)
