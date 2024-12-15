{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
import Data.List ( sort, groupBy, group, sortOn ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split, join) --, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust, catMaybes, isNothing )
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
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 15 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input15.txt"
  -- content <- readFile "Input24/test15_1.txt"
  -- content <- readFile "Input24/test15_2.txt"


  -- regroup lines separated by [""]
  let contLines = filter (/= [""]) $ groupBy (\x y -> x /= "" && y /= "") (lines content)
  -- print $ "contLines=" ++ show contLines

  let gridMap = readGrid (head contLines) :: GridMap
  let posRob = head $ Map.keys (Map.filter (=='@') (snd gridMap))
  print $ "posRob=" ++ show posRob
  -- remove robot
  let startMap = (fst gridMap, Map.insert posRob '.' (snd gridMap))

  let cmds = join "" (contLines !! 1)
  -- print $ "cmds=" ++ show cmds

  let (nbIt, (posFinal, gridFinal)) = applyMoves (-1, 0) (posRob, startMap) cmds
  print $ "posFinal=" ++ show posFinal
  putStrLn (mapToStr gridFinal)

  let allGPS = gpsOfMap gridFinal
  -- print $ "allGPS=" ++ show allGPS

  let pRes = sum allGPS
  putStrLn $ "Answer 1> " ++ show pRes


  let boxMap = readGrid (map biggerWarehouse (head contLines))
  -- putStrLn (mapToStr boxMap)
  let posboxInit = head $ Map.keys (Map.filter (=='@') (snd boxMap))
  -- print $ "posRob=" ++ show posboxInit
  -- remove robot
  let startboxMap = (fst boxMap, Map.insert posboxInit '.' (snd boxMap))

  let doMove nb = do
        let (nbIt, (boxposFinal, boxFinal)) = applyBoxPusherCmds (nb, 0) (posboxInit, startboxMap) cmds
        print $ "######### it=" ++ show nbIt
        print $ "posFinal=" ++ show boxposFinal
        putStrLn (niceWorld boxFinal boxposFinal)
        let boxGPS = sum $ gpsOfBoxMap boxFinal
        print $ "boxGPS=" ++ show boxGPS

  -- mapM_ doMove [5 .. length cmds]
  mapM_ doMove [length cmds]
  -- mapM_ doMove [300 .. 330]

  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type GridMap = GridMapCore Char
-- direction is 0:North, 1:West, 2:South, 3:East
type Dir = Int

niceDir :: Dir -> Char
niceDir 0 = '^'
niceDir 1 = '<'
niceDir 2 = 'v'
niceDir 3 = '>'

decodeDir '^' = 0
decodeDir '<' = 1
decodeDir 'v' = 2
decodeDir '>' = 3
decodeDir c = error ("Unknown Dir c=[" ++ [c] ++ "]")

nextCell :: Pos -> Dir -> Pos
nextCell (x,y) 0 = (x-1, y)
nextCell (x,y) 1 = (x, y-1)
nextCell (x,y) 2 = (x+1, y)
nextCell (x,y) 3 = (x, y+1)

-- find freeCell is a given direction
firstFree :: GridMap -> Pos -> Dir -> Maybe Pos
firstFree gridMap pos d = case Map.lookup nextPos grid of
  Nothing -> error ("Should not look at pos=" ++ show nextPos)
  Just '.' -> Just nextPos
  Just 'O' -> firstFree gridMap nextPos d
  Just '#' -> Nothing
  Just c   -> error ("Should not see " ++ [c] ++ " at pos=" ++ show nextPos )
  where
    grid = snd gridMap
    nextPos = nextCell pos d

moveRobot :: GridMap -> Pos -> Dir -> (Pos, GridMap)
moveRobot gridMap pos d
  | targetCell == '.' = (targetPos, gridMap)
  | targetCell == '#' = (pos, gridMap)
  | otherwise = case firstFree gridMap targetPos d of
      Nothing -> (pos, gridMap)
      Just freePos -> (targetPos, updatedMap)
        where
          removedObstacle = Map.insert targetPos '.' (snd gridMap)
          updatedMap = (fst gridMap, Map.insert freePos 'O' removedObstacle)
  where
    targetPos = nextCell pos d
    targetCell = snd gridMap Map.! targetPos

applyMoves :: (Int, Int) -> (Pos, GridMap) -> String -> (Int, (Pos, GridMap))
applyMoves (maxIt, curIt) (pos, gridMap) [] = (curIt, (pos,gridMap))
applyMoves (maxIt, curIt) (pos, gridMap) (d:ds)
  | maxIt < 0 = applyMoves (maxIt, curIt+1) (moveRobot gridMap pos (decodeDir d)) ds
  | curIt >= maxIt = (curIt, (pos, gridMap))
  | otherwise = applyMoves (maxIt, curIt+1) (moveRobot gridMap pos (decodeDir d)) ds


gpsOfMap :: GridMap -> [Int]
gpsOfMap (size, grid) = map (\(r,c) -> 100 * r + c) keysOfGoods
  where
    keysOfGoods = Map.keys (Map.filter (=='O') grid)


mapToStr :: GridMap -> String
mapToStr (size, grid) = chunks (snd size) $ map snd (Map.toList grid)

niceWorld :: GridMap -> Pos -> String
niceWorld (size, grid) pos = mapToStr (size, Map.insert pos '@' grid)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

biggerWarehouse [] = []
biggerWarehouse ('#':ls) = "##" ++ biggerWarehouse ls
biggerWarehouse ('O':ls) = "[]" ++ biggerWarehouse ls
biggerWarehouse ('.':ls) = ".." ++ biggerWarehouse ls
biggerWarehouse ('@':ls) = "@." ++ biggerWarehouse ls

freePlace '.' = True
freePlace c = False

-- find freeCell is a given direction
freeBox :: GridMap -> Pos -> Dir -> Maybe Pos
freeBox gridMap pos d = case Map.lookup nextPos grid of
  Nothing -> error ("Should not look at pos=" ++ show nextPos)
  Just '.' -> Just nextPos
  Just '[' -> freeBox gridMap nextPos d
  Just ']' -> freeBox gridMap nextPos d
  Just '#' -> Nothing
  Just c   -> error ("Should not see " ++ [c] ++ " at pos=" ++ show nextPos )
  where
    grid = snd gridMap
    nextPos = nextCell pos d

moveBoxes :: GridMap -> Pos -> Dir -> Maybe GridMap
moveBoxes (size, grid) (r1,c1) 1 = case freeBox (size, grid) (r1,c1) 1 of
  Nothing -> Nothing
  Just (fr, fc) -> if fc == (c1-1) then Just (size, grid)
                                   else Just (size, foldl opMoveLeft grid [fc .. (c1-1)])
    where opMoveLeft grid c = Map.insert (r1, c) (grid Map.! (r1,c+1)) grid
moveBoxes (size, grid) (r1,c1) 3 = case freeBox (size, grid) (r1,c1) 3 of
  Nothing -> Nothing
  Just (fr, fc) -> if fc == (fc+1) then Just (size, grid)
                                   else Just (size, foldl opMoveRight grid [fc, (fc-1) .. (c1+1)])
    where opMoveRight grid c = Map.insert (r1, c) (grid Map.! (r1,c-1)) grid

moveBoxUp :: GridMap -> [Pos] -> Maybe GridMap
moveBoxUp gridMap fromPos = case checkUp (snd gridMap) [] fromPos of
  Nothing -> Nothing
  Just [] -> Just (fst gridMap, foldl opMoveUp (snd gridMap) fromPos)
  Just posToCheck -> case moveBoxUp gridMap posToCheck of
    Nothing -> Nothing
    Just (size, newGrid) -> Just (size, foldl opMoveUp newGrid fromPos)
  where
    opMoveUp :: Map.Map Pos Char -> Pos -> Map.Map Pos Char
    opMoveUp grid p = Map.insert p '.' (Map.insert (nextCell p 0) (grid Map.! p) grid)

    checkUp :: Map.Map Pos Char -> [Pos] -> [Pos] -> Maybe [Pos]
    checkUp grid acc [] = Just (Set.toList $ Set.fromList acc)
    checkUp grid acc (pos:ps)
      | nextPlace == '.' = checkUp grid acc ps
      | nextPlace == '[' = checkUp grid (nextPos:(nextCell nextPos 3):acc) ps
      | nextPlace == ']' = checkUp grid (nextPos:(nextCell nextPos 1):acc) ps
      | nextPlace == '#' = Nothing
      where
        nextPos = nextCell pos 0
        nextPlace = grid Map.! nextPos

moveBoxDown :: GridMap -> [Pos] -> Maybe GridMap
moveBoxDown gridMap fromPos = case checkDown (snd gridMap) [] fromPos of
  Nothing -> Nothing
  Just [] -> Just (fst gridMap, foldl opMoveDown (snd gridMap) fromPos)
  Just posToCheck -> case moveBoxDown gridMap posToCheck of
    Nothing -> Nothing
    Just (size, newGrid) -> Just (size, foldl opMoveDown newGrid fromPos)
  where
    opMoveDown :: Map.Map Pos Char -> Pos -> Map.Map Pos Char
    opMoveDown grid p = Map.insert p '.' (Map.insert (nextCell p 2) (grid Map.! p) grid)

    checkDown :: Map.Map Pos Char -> [Pos] -> [Pos] -> Maybe [Pos]
    checkDown grid acc [] = Just (Set.toList $ Set.fromList acc)
    checkDown grid acc (pos:ps)
      | nextPlace == '.' = checkDown grid acc ps
      | nextPlace == '[' = checkDown grid (nextPos:(nextCell nextPos 3):acc) ps
      | nextPlace == ']' = checkDown grid (nextPos:(nextCell nextPos 1):acc) ps
      | nextPlace == '#' = Nothing
      where
        nextPos = nextCell pos 2
        nextPlace = grid Map.! nextPos

applyBoxPusherCmds :: (Int, Int) -> (Pos, GridMap) -> String -> (Int, (Pos, GridMap))
applyBoxPusherCmds (maxIt, curIt) (pos, gridMap) [] = (curIt, (pos,gridMap))
applyBoxPusherCmds (maxIt, curIt) (pos, gridMap) (d:ds)
  | maxIt < 0 = boxPush pos d
  | curIt >= maxIt = (curIt, (pos, gridMap))
  | otherwise = boxPush pos d
  where
    boxPush :: Pos -> Char -> (Int, (Pos, GridMap))
    boxPush pos cmd
      | cmd == '^' = case moveBoxUp gridMap [pos] of
          Nothing -> applyBoxPusherCmds (maxIt, curIt+1) (pos, gridMap) ds
          Just newGrid -> applyBoxPusherCmds (maxIt, curIt+1) (nextCell pos 0, newGrid) ds
      | cmd == '<' = case moveBoxes gridMap pos 1 of
          Nothing -> applyBoxPusherCmds (maxIt, curIt+1) (pos, gridMap) ds
          Just newGrid -> applyBoxPusherCmds (maxIt, curIt+1) (nextCell pos 1, newGrid) ds
      | cmd == 'v' = case moveBoxDown gridMap [pos] of
          Nothing -> applyBoxPusherCmds (maxIt, curIt+1) (pos, gridMap) ds
          Just newGrid -> applyBoxPusherCmds (maxIt, curIt+1) (nextCell pos 2, newGrid) ds
      | cmd == '>' = case moveBoxes gridMap pos 3 of
          Nothing -> applyBoxPusherCmds (maxIt, curIt+1) (pos, gridMap) ds
          Just newGrid -> applyBoxPusherCmds (maxIt, curIt+1) (nextCell pos 3, newGrid) ds

gpsOfBoxMap :: GridMap -> [Int]
gpsOfBoxMap (size, grid) = map (\(r,c) -> 100 * r + c) keysOfGoods
  where
    keysOfGoods = Map.keys (Map.filter (=='[') grid)
