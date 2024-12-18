{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
import Data.List ( find, sortOn, groupBy ) -- sortOn, groupBy, find, group, sort, sortBy _)
-- import Data.Tuple ( swap )
import Data.String.Utils ( split, join) --, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Data.List ( delete, sortOn ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
-- import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 18 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input18.txt"
  let sizeG = (70, 70)
  let nbObst = 1024
  -- content <- readFile "Input24/test18_1.txt"
  -- let sizeG = (6, 6)
  -- let nbObst = 12

  let obstacles = map parseObstacle (lines content)
  let baseGrid = (sizeG, Map.fromList [((r,c),'.') | r <- [-1 .. 1 + fst sizeG], c <- [-1 .. 1 + fst sizeG]])
  let gridInit = addBarrier baseGrid
  let gridObst = addObst gridInit (take nbObst obstacles)
  putStrLn (mapToStr gridObst)

  let aStarBegin = aStarInit gridObst (0,0)
  let solveA = aStarStep gridObst (Map.fromList [((0,0), 0)]) aStarBegin sizeG

  let pRes = snd (head $ snd solveA)
  putStrLn $ "Answer 1> " ++ show pRes

  let g01 = addObst gridObst [(1,1)]
  let b01 = aStarInit g01 (0,0)
  let s01 = aStarStep g01 (Map.fromList [((0,0), 0)]) b01 sizeG
  print $ "s01=" ++ show (snd s01)

  let g02 = addObst g01 [(6,1)]
  let b02 = aStarInit g02 (0,0)
  let s02 = aStarStep g02 (Map.fromList [((0,0), 0)]) b02 sizeG
  print $ "s02=" ++ show (snd s02)
  putStrLn (mapToStr g02)

  let failure = lookForFail (-1,0) gridObst (drop nbObst obstacles)
  print $ "failure=" ++ show failure




  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type GridMap = GridMapCore Char
type Dir = Int
type Cmd = Int
type State = Pos
type ResMap = Map.Map State Int -- State -> cost so far

parseObstacle :: String -> Pos
parseObstacle line = (read $ head tok, read $ tok !! 1)
  where
    tok = split "," line

addBarrier :: GridMap -> GridMap
addBarrier (size, gridMap) = (size, foldl (\g p -> Map.insert p ('#') g) gridMap
                               [(r,c) | r <- [-1 .. 1 + fst size], c <- [-1 .. 1 + fst size],
                                r == (-1) || c == (-1) || r == (1 + fst size) || (c == (1+ fst size))])

mapToStr :: GridMap -> String
mapToStr (size, grid) = chunks (3 + snd size) $ map snd (Map.toList grid)

addObst :: GridMap -> [Pos] -> GridMap
addObst (size, gridMap) posList = (size, foldl (\g p -> Map.insert p '#' g) gridMap posList)

-- A* **************************************************************************
advance :: State -> Dir -> Pos
advance (r,c) 0 = (r-1,c)
advance (r,c) 1 = (r,c-1)
advance (r,c) 2 = (r+1,c)
advance (r,c) 3 = (r,c+1)

applyCmd :: GridMap -> State -> Int -> Cmd -> Maybe (State, Int)
applyCmd (size, grid) pos curCost dir
  | nextCell == '#' = Nothing
  | otherwise       = Just (nextPos, curCost + 1)
  where
    nextPos = advance pos dir
    nextCell = grid Map.! nextPos

-- A* **************************************************************************
aStarInit :: GridMap -> State -> [(State, Int)]
aStarInit gridMap state = catMaybes (map (applyCmd gridMap state 0) [0, 1, 2, 3])

-- need goal pos
aStarStep :: GridMap -> ResMap -> [(State, Int)] -> Pos -> (ResMap, [(State, Int)])
aStarStep grid res [] goalPos = (res, []) -- no solution
aStarStep grid res toExplore goalPos
  | startState == goalPos = (res, [(startState, startCost)])
  | otherwise = aStarStep grid addedRes (foldr (addToExplore res) nexts newNext) goalPos
  where
    (next:nexts) = sortOn (heuristic goalPos) toExplore :: [(State, Int)]
    -- (nbRow, nbCol) = fst grid
    startState = fst next :: State
    startCost = snd next  :: Int
    newNext = catMaybes (map (applyCmd grid startState startCost) [0, 1, 2, 3])
    addedRes = Map.insert startState startCost res

-- add in Map (toExplore) only if inferior cost and not in closeList (ResMap)
addToExplore :: ResMap -> (State, Int) -> [(State, Int)] -> [(State, Int)]
addToExplore res state toExplore = case Map.lookup (fst state) res of
  Just _  -> toExplore
  Nothing -> case find (\s -> fst state == fst s) toExplore of
    Just (_, cost) -> if cost <= snd state
                         then toExplore
                         else state:toExplore
    Nothing        -> state:toExplore

-- heuristic path length funtion
heuristic :: Pos -> (State, Int) -> Int
heuristic (rgoal, cgoal) ((r,c), costSoFar) = costSoFar + abs (rgoal - r) + abs (cgoal - c)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

lookForFail :: (Int, Int) -> GridMap -> [Pos] -> (Int, Pos)
lookForFail (maxIt, curIt) gridMap (obst:os)
  | soluce == [] = (curIt, obst)
  | otherwise = lookForFail (maxIt, curIt+1) newGrid os
  where
    newGrid = addObst gridMap [obst]
    begin = aStarInit newGrid (0,0) --  `debug` ("add " ++ show curIt)
    soluce = snd (aStarStep newGrid (Map.fromList [((0,0), 0)]) begin (fst gridMap))
