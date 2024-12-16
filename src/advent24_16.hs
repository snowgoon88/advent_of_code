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
-- import Data.String.Utils ( split, join) --, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Data.List ( delete, sortOn ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
-- *********************************************************************************** DEBUG
import Debug.Trace ( trace ) -- trace :: String > a -> a
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug = flip trace

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 16 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input16.txt"
  -- content <- readFile "Input24/test16_1.txt"
  -- content <- readFile "Input24/test16_2.txt"

  let gridMap = readGrid (lines content)
  let startRobPos = head $ Map.keys (Map.filter (== 'S') (snd gridMap))
  let goalPos = head $ Map.keys (Map.filter (== 'E') (snd gridMap))
  print $ "startRobPos=" ++ show startRobPos
  print $ "goalPos=" ++ show goalPos

  let aStarBegin = aStarInit gridMap (startRobPos, 3)
  print $ "aStarBegin=" ++ show aStarBegin

  let solveA = aStarStep gridMap (Map.fromList [((startRobPos, 3), 0)]) aStarBegin goalPos
  let pRes = snd (head $ snd solveA)
  putStrLn $ "Answer 1> " ++ show pRes

  -- print $ "resMap5= " ++ show (take 5 (Map.toList (fst solveA)))
  -- writeFile "resmap_2416.txt" (show $ Map.toList (fst solveA))

  -- print $ "lenPath=" ++ show (length (snd solveA))

  let stateEnd = head $ snd solveA
  -- print $ "stateEnd=" ++ show stateEnd


  -- let prevStateCost = map (\s -> (s, fromMaybe (snd stateEnd) (Map.lookup s (fst solveA)))) (prevStates (fst stateEnd))
  -- let bestPrev = head $ sortOn snd prevStateCost
  -- print $ "prevStateCost=" ++ show prevStateCost
  -- print $ "bestPrev=" ++ show bestPrev

  -- let pathSol = buildPathFrom (fst solveA) [stateEnd] stateEnd (startRobPos, 3)
  -- print $ "pathSol=" ++ show pathSol


  let solveB = breadFirst gridMap (Map.fromList [((startRobPos, 3), 0)]) aStarBegin pRes
  -- print $ "solveB=" ++ show solveB

  let validEnds = filter (\s -> fst s == goalPos) (Map.keys solveB)
  -- print $ "validEnds=" ++ show validEnds

  -- let all_7_4 = filter (\s -> (fst s) == (7, 4)) (Map.keys solveB)
  -- print $ "all_7_4=" ++ show all_7_4
  -- print $ "      ->" ++ show (map (solveB Map.!) all_7_4)

  -- let prev_7_5_3 = map (\s -> (s, fromMaybe 4010 (Map.lookup s solveB))) (prevStates ((7, 5), 3))
  --   -- consider all minimums
  -- let best_7_5_3 = filter (\s -> (snd s < 4010)) (sortOn snd prev_7_5_3)
  -- print $ "prev_7_5_3=" ++ show prev_7_5_3
  -- print $ "best_7_5_3" ++ show best_7_5_3


  -- let debugPath nb = do
  --       let allCells = allPathFrom (nb, 0) solveB [] (map (\s -> (s,pRes)) validEnds)
  --       print $ "After " ++ show nb ++ " ============================"
  --       print $ "allCells=" ++ show allCells

  -- mapM_ debugPath [50..65]

  let (it, allCells, leftover) = allPathFrom (-1, 0) solveB [] (map (\s -> (s,pRes)) validEnds)
  let allPos = Set.fromList (map (fst . fst) allCells)
  let cRes = Set.size allPos
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Dir = Int
data Cmd = A | R | L deriving (Eq, Ord, Show)
type GridMap = GridMapCore Char

rotateLeft :: Dir -> Dir
rotateLeft d = mod (d+1) 4
rotateRight :: Dir -> Dir
rotateRight d = mod (d-1) 4

type State = (Pos, Dir)
type ResMap = Map.Map State Int -- State -> cost so far

advance :: State -> Pos
advance ((r,c), 0) = (r-1,c)
advance ((r,c), 1) = (r,c-1)
advance ((r,c), 2) = (r+1,c)
advance ((r,c), 3) = (r,c+1)

applyCmd :: GridMap -> State -> Int -> Cmd -> Maybe (State, Int)
applyCmd (size, grid) (pos, dir) curCost A
  | nextCell == '#' = Nothing
  | otherwise       = Just ((nextPos, dir), curCost + 1)
  where
    nextPos = advance (pos, dir)
    nextCell = grid Map.! nextPos
applyCmd (size, grid) (pos, dir) curCost R = Just ((pos, rotateRight dir), curCost+1000)
applyCmd (size, grid) (pos, dir) curCost L = Just ((pos, rotateLeft dir), curCost+1000)

-- A* **************************************************************************
aStarInit :: GridMap -> State -> [(State, Int)]
aStarInit gridMap state = catMaybes (map (applyCmd gridMap state 0) [A, R, L])

-- need goal pos
aStarStep :: GridMap -> ResMap -> [(State, Int)] -> Pos -> (ResMap, [(State, Int)])
aStarStep grid res toExplore goalPos
  | (fst startState) == goalPos = (res, [(startState, startCost)])
  | otherwise = aStarStep grid addedRes (foldr (addToExplore res) nexts newNext) goalPos
  where
    (next:nexts) = sortOn (heuristic goalPos) toExplore :: [(State, Int)]
    -- (nbRow, nbCol) = fst grid
    startState = fst next :: State
    startCost = snd next  :: Int
    newNext = catMaybes (map (applyCmd grid startState startCost) [A, R, L])
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
heuristic (rgoal, cgoal) (((r,c), _), costSoFar) = costSoFar + abs (rgoal - r) + abs (cgoal - c)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- extraire le chemin de ResMap, en remontant depuis le but
-- et ensuite chercher quand il y a des cases libres à droites et à gauche.

-- possible previous states from a given State
prevStates :: State -> [State]
prevStates (pos, dir) = [(pos, rotateLeft dir), (pos, rotateRight dir), (prevPos, dir)]
  where
    prevPos = advance (pos, mod (dir+2) 4)

buildPathFrom :: ResMap -> [(State,Int)] -> (State, Int) -> State -> [(State, Int)]
buildPathFrom resMap acc stateCost beginState
  | fst bestPrev == beginState = bestPrev:acc
  | otherwise = buildPathFrom resMap (bestPrev:acc) bestPrev beginState -- `debug` ("best=" ++ show bestPrev)
  where
    -- take default value of Map.Lookup as the cost of the current stateCost
    prevStateCost = map (\s -> (s, fromMaybe (snd stateCost) (Map.lookup s resMap))) (prevStates (fst stateCost))
    bestPrev = head $ sortOn snd prevStateCost

-- alternatePath :: (ResMap -> [(State,Int)] -> Int) -> ResMap -> (State, Int) -> [(State, Int)] -> Int -> (ResMap, [(State, Int)])
-- alternatePath (resGlobal, onPath, optCost) resLocal fromState toExplore
--   -- nothing left to Explore
--   | toExplore == [] = (resMap, onPath)
--   -- nextBest is already to far from goal
--   | curCost > optCost = alternatePath (resGlobal, onPath, optCOst) resLocal nextsœ optCost
--   -- TODO reached again knownPath to Goal
--   | elem next onPath = alternatePath resMap onPath nexts optCost
-- where
--     (next:nexts) = sortOn (heuristic goalPos) toExplore :: [(State, Int)]
--     curState = fst next :: State
--     curCost = snd next  :: Int

breadFirst :: GridMap -> ResMap -> [(State, Int)] -> Int -> ResMap
breadFirst gridMap resMap toExplore cuttingCost
  | toExplore == [] = resMap
  | startCost > cuttingCost = breadFirst gridMap resMap nexts cuttingCost
  | otherwise = breadFirst gridMap addedRes (foldr (addToExplore resMap) nexts newNext) cuttingCost
  where
    (next:nexts) = sortOn snd toExplore :: [(State, Int)]
    -- (nbRow, nbCol) = fst grid
    startState = fst next :: State
    startCost = snd next  :: Int
    inRes pos = Map.member startState resMap

    newNext = catMaybes (map (applyCmd gridMap startState startCost) [A, R, L])
    addedRes = Map.insert startState startCost resMap

allPathFrom :: (Int, Int) -> ResMap -> [(State, Int)] -> [(State, Int)] -> (Int, [(State, Int)], [(State, Int)])
allPathFrom (_, curIt) resMap acc [] = (curIt, acc, [])
allPathFrom (maxIt, curIt ) resMap acc (stateCost:scs)
  | maxIt < 0 = allPathFrom (maxIt, curIt+1) resMap (stateCost:acc) toExplore
  | curIt >= maxIt = (curIt, acc, stateCost:scs)
  | otherwise = allPathFrom (maxIt, curIt+1) resMap (stateCost:acc) toExplore
  where
    -- take default value of Map.Lookup as the cost of the current stateCost
    prevStateCost = map (\s -> (s, fromMaybe (snd stateCost) (Map.lookup s resMap))) (prevStates (fst stateCost))
    -- consider all minimums
    bestPrevs = filter (validPrev stateCost) (sortOn snd prevStateCost) --`debug` ("bestPrevs=" ++ show bestPrevs)
    toExplore = Set.toList ( Set.fromList (scs ++ bestPrevs)) --`debug` ("toExplore=" ++ show toExplore)
    validPrev ((sto, dirto), costto) ((sfrom, dirfrom), costfrom) = ((dirfrom == dirto) && (costfrom == (costto - 1)))
                                                                   || (costfrom == (costto - 1000))
