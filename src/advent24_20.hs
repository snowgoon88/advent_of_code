{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
import Data.String.Utils ( split, join, startswith) --, startswith, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( find, delete, sortOn, groupBy ) -- delete, sortOn
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
  putStrLn "** Advent 2024 - Day 20 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input20.txt"
  -- content <- readFile "Input24/test20_1.txt"

  let sizegridMap = readGrid (lines content) :: GridMap
  let endPos = head $ Map.keys (Map.filter (== 'E') (snd sizegridMap)) :: Pos
  let startPos = head $ Map.keys (Map.filter (== 'S') (snd sizegridMap)) :: Pos
  print $ "startPos=" ++ show startPos
  print $ "endPos=" ++ show endPos

  let aStarMapInit = aStarInit sizegridMap endPos

  let solveSol = aStarStep sizegridMap (Map.fromList [(endPos, 0)]) aStarMapInit startPos

  let bestPathLen = snd ( head $ snd solveSol )
  print $ "bestPathLen=" ++ show bestPathLen

  let bestPath = buildPathFrom (fst solveSol) [(startPos, bestPathLen)] (startPos, bestPathLen) endPos
  -- print $ "bestPath=" ++ show bestPath

  let possCheat = Set.toList (Set.fromList (concat $ map (findCheat sizegridMap) bestPath))
  -- print $ "possCheat=" ++ show possCheat
  print $ "possCheat len=" ++ show ( length possCheat)

  let bfRes = bfGrow (bestPathLen ,0) sizegridMap (Map.fromList [(endPos, 0)]) [endPos]
  -- print $ "bfRes=" ++ show (fst bfRes, Map.size (snd bfRes))

  let cheats = filter (goodCheat (snd bfRes) 99) possCheat
  -- print $ "cheats=" ++ show cheats

  let infoCheats = map (\(pos, cost) -> ((pos,cost), (snd bfRes) Map.! pos) ) cheats
  print "====="
  -- print $ "infoCheats" ++ show ((sortOn (\((p,c),score) -> c-score)) infoCheats)
  print $ "len infoCheats=" ++ show (length cheats)


  let altCheats = addCheat sizegridMap (fst solveSol) [] 99 bestPath
  -- print $ "altCheats=" ++ show altCheats
  print $ "altCheatsLen=" ++ show (length altCheats)

  let pRes = length altCheats
  putStrLn $ "Answer 1> " ++ show pRes

  -- let potPos = potentialPos 20 startPos
  -- print $ "potPos=" ++ show potPos
  -- let uniquePot = uniquePotential potPos
  -- print $ "uniquePot=" ++ show uniquePot
  -- print $ "potPosLen=" ++ show (length potPos)
  -- print $ "potPosLen=" ++ show (length uniquePot)

  -- print $ "from start=" ++ show startPos ++ ", cost=" ++ show bestPathLen
  -- let potCheat = reachableCheat (fst solveSol) 20 73 (startPos, bestPathLen)
  -- print $ "potCheat=" ++ show potCheat

  let allCheat = concat $ map (reachableCheat (fst solveSol) 20 99) bestPath
  -- let sortedCheat = sortOn snd allCheat
  -- let groupedCheat = groupBy (\c1 c2 -> snd c1 == snd c2) sortedCheat
  -- let resCheat = map (\g -> (length g, snd $ head g)) groupedCheat
  -- print $ "resCheat=" ++ show resCheat

  let cRes = length allCheat
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type GridMap = GridMapCore Char
type Dir = Int
type Cmd = Int
type State = Pos
type ResMap = Map.Map State Int -- State -> cost so far

-- first, solve from End to Start  with Astar => store dist from End

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


-- extraire le chemin de ResMap, en remontant depuis le but
-- et ensuite chercher quand il y a des cases libres à droites et à gauche.

-- possible previous states from a given State
prevStates :: State -> [State]
prevStates pos = map (advance pos) [0, 1, 2, 3]

buildPathFrom :: ResMap -> [(State,Int)] -> (State, Int) -> State -> [(State, Int)]
buildPathFrom resMap acc stateCost beginState
  | fst bestPrev == beginState = bestPrev:acc
  | otherwise = buildPathFrom resMap (bestPrev:acc) bestPrev beginState -- `debug` ("best=" ++ show bestPrev)
  where
    -- take default value of Map.Lookup as the cost of the current stateCost
    prevStateCost = map (\s -> (s, fromMaybe (snd stateCost) (Map.lookup s resMap))) (prevStates (fst stateCost))
    bestPrev = head $ sortOn snd prevStateCost

-- findCheat
findCheat :: GridMap -> (Pos, Int) -> [(Pos, Int)]
findCheat gridMap (pos, cost) = map (, cost) (catMaybes $ map findCheatInDir [0, 1, 2, 3])
  where
    findCheatInDir dir
      | isObst gridMap adv1 && isFree gridMap adv2 = Just adv2
      | otherwise = Nothing
      where
        adv1 = advance pos dir
        adv2 = advance adv1 dir

addCheat :: GridMap -> ResMap -> [((Pos,Int), (Pos,Int))] -> Int -> [(Pos, Int)] -> [((Pos, Int), (Pos, Int))]
addCheat gridMap resMap acc minDiff [] = acc
addCheat gridMap resMap acc minDiff ((pos,cost):pcs) = addCheat gridMap resMap (acc++toAdd) minDiff pcs
  where
    goodCheatPos = filter (isGoodCheat resMap minDiff) (findCheat gridMap (pos,cost))
    toAdd = map (\(p,c) -> ((pos,cost),(p, resMap Map.! p))) goodCheatPos

isGoodCheat :: ResMap -> Int -> (Pos, Int) -> Bool
isGoodCheat resMap minDiff (posCheat, costOri) = case Map.lookup posCheat resMap of
  Nothing -> False
  Just costCheat -> (costCheat+2) < (costOri - minDiff)

isObst :: GridMap -> Pos -> Bool
isObst gridMap pos = getValMap gridMap pos '#' == '#'
isFree :: GridMap -> Pos -> Bool
isFree gridMap pos = not ( isObst gridMap pos )

bfGrow :: (Int, Int) -> GridMap -> ResMap -> [Pos] -> (Int, ResMap)
bfGrow (maxIt, curIt) gridMap resMap toExplore
  | curIt >= maxIt = (curIt, resMap)
  | otherwise = bfGrow (maxIt, curIt+1) gridMap updatedResMap expanded
  where
    expand pos = filter (\p -> (isFree gridMap p) && (not (Map.member p resMap))) (map (advance pos) [0..3])
    expanded = Set.toList (Set.fromList $ concat (map expand toExplore))
    updatedResMap = foldl (\m p -> Map.insert p (curIt+1) m) resMap expanded

goodCheat :: ResMap -> Int -> (Pos, Int) -> Bool
goodCheat bfMap minScore (pos, cost) = case Map.lookup pos bfMap of
  Nothing -> False
  Just dist -> dist < (cost - minScore)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- from a pathPos,cost -> reachablePos, costOri+dist
reachableCheat :: ResMap -> Int -> Int -> (Pos,Int) -> [(Pos,Int)]
reachableCheat resMap maxDist minDiff ((r,c),cost) = filterPotential [] uniquePotential
  where
    potentialPos = [((r+x, c+y), abs x + abs y) | x <- [-maxDist .. maxDist], y <- [-maxDist .. maxDist],
                                                             (abs x + abs y) <= maxDist]
    uniquePotential = Set.toList (Set.fromList potentialPos)
    filterPotential :: [(Pos,Int)] -> [(Pos,Int)] -> [(Pos,Int)]
    filterPotential acc [] = acc
    filterPotential acc ((pos, d):ps) = case Map.lookup pos resMap of
      Nothing -> filterPotential acc ps
      Just costOpti -> if (costOpti+d) < (cost - minDiff)
                          then filterPotential ((pos, cost - (costOpti+d)):acc) ps
                          else filterPotential acc ps

-- hamming :: Pos -> Pos -> Int
-- hamming (r1,c1) (r2,c2) = abs (r1 - r2) + abs (c1 - c2)


potentialPos maxDist (r,c) = [((r+x, r+y), abs x + abs y) | x <- [-maxDist .. maxDist], y <- [-maxDist .. maxDist],
                                                             (abs x + abs y) <= maxDist]
uniquePotential lPos = Set.toList (Set.fromList lPos)

-- filterPotential resMap acc cost minDiff [] = acc
-- filterPotential resMap acc ((pos, d):ps) = case Map.lookup pos resMap of
--       Nothing -> filterPotential acc ps
--       Just costOpti -> if (costOpti+d) < (cost - minDiff)
--                           then filterPotential ((pos, costOpti+d):acc) ps
--                           else filterPotential acc ps
