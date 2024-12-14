{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
-- import Data.List ( sort ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust, catMaybes )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
import qualified Data.Set as Set
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


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 10 Part 1 & 3                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input10.txt"
  -- content <- readFile "Input24/test10_1.txt"

  let (size, grid) = readGrid (lines content)

  -- let reached = reachedPos (size, grid)
  -- print $ "reached=" ++ show reached

  let valMap = (size, Map.fromList [((r, c), []) | r <- [0..(fst size - 1)], c <- [0 .. (snd size - 1)]])

  let initValMap = initVal valMap (size, grid)
  -- print $ "initValMap=" ++ show initValMap

  -- let step8 = diffuseVal initValMap (size, grid) ('8', '9')
  -- print $ "step8=" ++ show step8

  let reachedH = reachedHeads initValMap (size, grid)
  -- print $ "reachedH=" ++ show reachedH

  let keys0 = Map.keys (Map.filter (== '0') grid)
  let reachedBy0 = map ((snd reachedH) Map.!) keys0
  -- print $ "reachedBy0=" ++ show reachedBy0

  let pRes = length (concat reachedBy0)

  -- let step7 = diffuseVal step8 (size, grid) ('7', '8')
  -- print $ "step7=" ++ show step7
  -- let step6 = diffuseVal step7 (size, grid) ('6', '7')
  -- let step5 = diffuseVal step6 (size, grid) ('5', '6')
  -- let step4 = diffuseVal step5 (size, grid) ('4', '5')
  -- let step3 = diffuseVal step4 (size, grid) ('3', '4')
  -- let step2 = diffuseVal step3 (size, grid) ('2', '3')
  -- let step1 = diffuseVal step2 (size, grid) ('1', '2')
  -- let step0 = diffuseVal step1 (size, grid) ('0', '1')
  -- print $ "step0=" ++ show step0
  putStrLn $ "Answer 1> " ++ show pRes


  let hikeMap = (size, Map.fromList [((r, c), 0) | r <- [0..(fst size - 1)], c <- [0 .. (snd size - 1)]]) :: HikeMap

  let initHikeMap = initHike hikeMap (size, grid)
  -- print $ "initValMap=" ++ show initValMap

  -- let step8 = diffuseVal initValMap (size, grid) ('8', '9')
  -- print $ "step8=" ++ show step8

  let allHikes = reachedHike initHikeMap (size, grid)
  -- print $ "reachedH=" ++ show reachedH

  -- let keys0 = Map.keys (Map.filter (== '0') grid)
  let hikesFrom0 = map ((snd allHikes) Map.!) keys0
  -- print $ "hikesFrom0=" ++ show hikesFrom0
  -- print $ "len hikesFrom0=" ++ show (sum hikesFrom0)

  let cRes = sum hikesFrom0
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type GridMap = GridMapCore Char
type ValMap = GridMapCore [Pos] -- reachable Heads from here

-- reachedPos gridMap = foldl (opStep gridMap) (Map.keys $ Map.filter (== '0') (snd gridMap))
--                                             ['1', '2', '3', '4', '5', '6', '7', '8', '9']

-- opStep :: GridMap -> [Pos] -> Char -> [Pos]
-- opStep gridMap posList nChar = concat (map (neighbors gridMap nChar) posList)

-- neighbors :: GridMap -> Char -> Pos -> [Pos]
-- neighbors gridMap nChar pos = filter (isCharAt gridMap nChar) (map (addDir pos)
--                                                                [(-1, 0), (1, 0), (0, -1), (0, 1)])

-- isCharAt (size, grid) nChar pos
--   | isValidPos size pos = grid Map.! pos == nChar
--   | otherwise           = False

initVal :: ValMap -> GridMap -> ValMap
initVal (sizeV, valMap) (sizeG, gridMap) = (sizeV, newVal)
  where
    posNine = Map.keys (Map.filter ('9' ==) gridMap)
    newVal = modifyMap valMap (\p -> [p]) posNine

reachedHeads :: ValMap -> GridMap -> ValMap
reachedHeads valMap gridMap = foldl (\vM (fromC, toC) -> diffuseVal vM gridMap (fromC, toC))
                                    valMap [('8', '9'), ('7', '8'), ('6', '7'), ('5', '6'), ('4', '5'), ('3', '4'), ('2', '3'), ('1', '2'), ('0', '1')]

diffuseVal :: ValMap -> GridMap -> (Char, Char) -> ValMap
diffuseVal (sizeV, values) (sizeG, grid) (cChar, nChar) = (sizeV, newVal)
  where
    posCenter = Map.keys (Map.filter (cChar ==) grid)
    newVal = modifyMap values (valueNeighbors values (sizeG, grid) nChar) posCenter


valueNeighbors :: Map.Map Pos [Pos] -> GridMap -> Char -> Pos -> [Pos]
valueNeighbors values gridMap nChar pos = Set.toList $ Set.fromList $ concat (map (\dir -> getVal values gridMap nChar (addDir pos dir))
                                               [(-1, 0), (1, 0), (0, -1), (0, 1)])

getVal :: Map.Map Pos [Pos] -> GridMap -> Char -> Pos -> [Pos]
getVal values (size, grid) nChar pos
  | isValidPos size pos = if grid Map.! pos == nChar then values Map.! pos
                                                     else []
  | otherwise = []

modifyMap :: Map.Map Pos [Pos] -> (Pos -> [Pos]) -> [Pos] -> Map.Map Pos [Pos]
modifyMap valMap _ [] = valMap
modifyMap valMap func (pos:ps) = modifyMap (Map.insert pos (func pos) valMap) func ps

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
type HikeMap = GridMapCore Int-- path through here


initHike :: HikeMap -> GridMap -> HikeMap
initHike (sizeV, hikeMap) (sizeG, gridMap) = (sizeV, newHike)
  where
    posNine = Map.keys (Map.filter ('9' ==) gridMap)
    newHike = modifyHike hikeMap (const 1) posNine

reachedHike :: HikeMap -> GridMap -> HikeMap
reachedHike hikeMap gridMap = foldl (\vM (fromC, toC) -> diffuseHike vM gridMap (fromC, toC))
                                    hikeMap [('8', '9'), ('7', '8'), ('6', '7'), ('5', '6'), ('4', '5'), ('3', '4'), ('2', '3'), ('1', '2'), ('0', '1')]

diffuseHike :: HikeMap -> GridMap -> (Char, Char) -> HikeMap
diffuseHike (sizeV, hikes) (sizeG, grid) (cChar, nChar) = (sizeV, newHike)
  where
    posCenter = Map.keys (Map.filter (cChar ==) grid)
    newHike = modifyHike hikes (hikeNeighbors hikes (sizeG, grid) nChar) posCenter


hikeNeighbors :: Map.Map Pos Int -> GridMap -> Char -> Pos -> Int
hikeNeighbors hikes gridMap nChar pos = sum (map (\dir -> getHikes hikes gridMap nChar (addDir pos dir))
                                               [(-1, 0), (1, 0), (0, -1), (0, 1)])

getHikes :: Map.Map Pos Int -> GridMap -> Char -> Pos -> Int
getHikes hikes (size, grid) nChar pos
  | isValidPos size pos = if grid Map.! pos == nChar then hikes Map.! pos
                                                     else 0
  | otherwise = 0

modifyHike :: Map.Map Pos Int -> (Pos -> Int) -> [Pos] -> Map.Map Pos Int
modifyHike hikeMap _ [] = hikeMap
modifyHike hikeMap func (pos:ps) = modifyHike (Map.insert pos (func pos) hikeMap) func ps
