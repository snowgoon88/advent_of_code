{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

--import qualified MyParser as MP
import MyParser ( GridMap, readGrid )
import Data.List ( sortOn, find ) --group ) --, sort, , sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( replace, split )
-- import qualified Data.Map.Strict as Map
import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 17 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input17.txt"
  -- content <- readFile "Input23/test17_1.txt"

  let charGrid = readGrid (lines content)
  let grid = (fst charGrid, readAsDigit (snd charGrid))
  let costE = applyCmd grid (0,0) 0 E
  print $ "costE=" ++ show costE

  let aStarBegin = aStarInit grid
  -- print $ "aStarBegin=" ++ show aStarBegin

  -- let step01 = aStarStepDebug 10000 0 grid Map.empty (aStarInit grid)
  -- print "__Step __________________"
  -- print $ niceExplore (fst grid) (snd step01)
  -- print "  ***********************"
  -- print $ niceRes (fst grid) (fst step01)
  -- print $ show (length (fst step01))

  let goal = aStarStep grid Map.empty (aStarInit grid)
  let pRes = snd (head $ snd goal)
  putStrLn $ "Answer 1> " ++ show pRes

  let costAdv = wobbleCmd grid (0,0) 0 E
  print $ "costAdv=" ++ show costAdv
  let costAdv2 = wobbleCmd grid (0,0) 0 S
  print $ "costAdv2=" ++ show costAdv2

  let aStarBeginAdv = aStarInitAdv
  let goalAdv = aStarStepAdv grid Map.empty aStarBeginAdv

  let cRes = snd (head $ snd goalAdv)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Dir = N | W | S | E deriving (Eq, Ord, Show)
type Cmd = (Dir, Int)
type Pos = (Int, Int) -- (row, col)
type GridIntMap = ((Int, Int), Map.Map Pos Int)

type State = (Pos, Cmd)
type ResMap = Map.Map State Int -- State -> cost so far (+ heuristics ?)

readAsDigit grid = Map.map digitToInt grid

-- A* step : manage list of next and already seen (ResMap)
aStarInit :: GridIntMap -> [(State, Int)]
aStarInit grid = concat $ map (applyCmd grid (0, 0) 0) firstCmd

aStarStep :: GridIntMap -> ResMap -> [(State, Int)] -> (ResMap, [(State, Int)])
aStarStep grid res toExplore
  | fst startState == (nbRow - 1, nbCol - 1) = (res, [(startState, startCost)])
  | otherwise = aStarStep grid addedRes (foldr (addToExplore res) nexts newNext)
  where
    (next:nexts) = sortOn (heurist (fst grid)) toExplore
    (nbRow, nbCol) = fst grid
    startState = fst next
    startCost = snd next
    newNext = concat $ map (applyCmd grid (fst startState) startCost) (nextCmd (snd startState))
    addedRes = Map.insert startState startCost res


niceRes size res = map (niceState size) (Map.toList res)
niceExplore :: (Int, Int) -> [(State, Int)] -> [String]
niceExplore size toExplore = map (niceState size) (sortOn (heurist size) toExplore)
niceState size (state, cost) = "P:" ++ show (fst state) ++ " - " ++ show (snd state) ++ " => " ++ show cost ++ " / " ++ show (heurist size (state, cost))

-- aStarStepDebug :: Int -> Int -> GridIntMap -> ResMap -> [(State, Int)] -> (ResMap, [(State, Int)])
-- aStarStepDebug maxStep nbStep grid res toExplore
--   | nbStep >= maxStep = (res, toExplore)
--   | fst startState == (nbRow - 1, nbCol - 1) = (res, [(startState, startCost)])
--   | otherwise = case Map.lookup startState res of
--     Just cost -> if cost <= startCost
--                     then aStarStepDebug maxStep (nbStep + 1) grid res nexts
--                     else aStarStepDebug maxStep (nbStep + 1) grid updatedRes (newNext ++ nexts)
--     Nothing -> aStarStepDebug maxStep (nbStep + 1) grid addedRes (newNext ++ nexts)
--   where
--     (next:nexts) = sortOn (heurist (fst grid)) toExplore
--     (nbRow, nbCol) = fst grid
--     startState = fst next
--     startCost = snd next
--     newNext = concat $ map (applyCmd grid (fst startState) startCost) (nextCmd (snd startState))
--     updatedRes = Map.adjust (const startCost) startState res
--     addedRes = Map.insert startState startCost res

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
heurist (nbRow, nbCol) (state, costSoFar) = costSoFar + (nbRow - row - 1) + (nbCol - col - 1)
  where (row, col) = fst state

-- generate list of ((reached Pos, used Cmd), cost of action)
applyCmd :: GridIntMap -> Pos -> Int -> Dir -> [((Pos, Cmd), Int)]
applyCmd (size, grid) (rowS, colS) curCost dir = zip (zip newPos (map (dir,) [1.. length newPos]))
                                                 (drop 1 $ scanl (+) curCost costs)
  where
    newPos = steps size (rowS, colS) dir 3
    costs = map (grid Map.!) newPos

nextCmd :: Cmd -> [Dir]
nextCmd (N, _) = [W, E]
nextCmd (S, _) = [W, E]
nextCmd (E, _) = [N, S]
nextCmd (W, _) = [N, S]

firstCmd :: [Dir]
firstCmd = [S, E]

-- generate a list of reachable Pos going in Dir from Pos
steps :: (Int, Int) -> Pos -> Dir -> Int -> [Pos]
steps sizes (rowS, colS) N nb = map (\i -> (i, colS)) [rowS-1, rowS-2 ..(max 0 (rowS-nb))]
steps sizes (rowS, colS) W nb = map (\i -> (rowS, i)) [colS-1, colS-2 ..(max 0 (colS-nb))]
steps (nbRow, _) (rowS, colS) S nb = map (\i -> (i, colS)) [rowS+1, rowS+2 ..(min (rowS+nb) (nbRow-1))]
steps (_, nbCol) (rowS, colS) E nb = map (\i -> (rowS, i)) [colS+1, colS+2 ..(min (colS+nb) (nbCol-1))]


-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- take action after 3 moves up to 10
wobbleCmd (size, grid) (rowS, colS) curCost dir = drop 3 nextStatesCost
  where
    allNextPos = steps size (rowS, colS) dir 10
    costs = map (grid Map.!) allNextPos
    nextStatesCost = zip (zip allNextPos (map (dir,) [1.. length allNextPos]))
                                                 (drop 1 $ scanl (+) curCost costs)

aStarInitAdv :: [(State, Int)]
aStarInitAdv = [(((0, 0), (E, 0)), 0), (((0, 0), (S, 0)), 0)]

aStarStepAdv :: GridIntMap -> ResMap -> [(State, Int)] -> (ResMap, [(State, Int)])
aStarStepAdv grid res toExplore
  | fst startState == (nbRow - 1, nbCol - 1) = (res, [(startState, startCost)])
  | otherwise = aStarStepAdv grid addedRes (foldr (addToExplore res) nexts newNext)
  where
    (next:nexts) = sortOn (heurist (fst grid)) toExplore
    (nbRow, nbCol) = fst grid
    startState = fst next
    startCost = snd next
    newNext = concat $ map (wobbleCmd grid (fst startState) startCost) (nextCmd (snd startState))
    addedRes = Map.insert startState startCost res
