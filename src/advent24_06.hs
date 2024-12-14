{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
-- import Data.List ( sort, group, sortOn, groupBy ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
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
import Debug.Trace ( trace ) -- trace :: String > a -> a
import GHC.Types.TyThing (MonadThings(lookupThing))
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug = flip trace

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 06 Part 1 & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input06.txt"
  -- content <- readFile "Input24/test06_1.txt"

  let gridMap = readGrid (lines content)
  let posStart = findStart gridMap '^'
  print $ "posStart=" ++ show posStart
  let guardStart = (posStart, (-1, 0))
  let pathList = computePath gridMap [guardStart] guardStart
  -- print $ "pathList=" ++ show pathList
  -- print $ "len patList=" ++ show (length pathList)

  let pathSet = Set.fromList (map fst pathList)
  -- print $ "pathSet=" ++ show (Set.toList pathSet)
  let pRes = Set.size pathSet

  putStrLn $ "Answer 1> " ++ show pRes

  -- let obstGrid = (fst gridMap, Map.insert (6,3) '#' (snd gridMap))
  -- let pathObst = computePath obstGrid [guardStart] guardStart
  -- print $ "pathObst=" ++ show pathObst

  let obstList = computeLoopPos gridMap [] [] guardStart
  -- let nbStart = length (filter (== (fst guardStart)) obstList)
  -- print $ "nbStart=" ++ show nbStart

  let obsSet = Set.fromList obstList
  print $ "obstSize=" ++ show (Set.size obsSet)

  -- -- let replacedStr = map (\p -> (snd gridMap) Map.! p) obstList
  -- -- print replacedStr

  -- -- print $ "obsList=" ++ show obstList

  -- let pathPos = map fst pathList
  -- let strange = filter (\p -> not (elem p pathPos)) obstList
  -- print $ "strange=" ++ show strange
  -- print $ "len strange=" ++ show (length strange)

  -- -- let allValid = all (map (isValidPos (fst gridMap) obstList))
  -- -- print $ "allValid=" ++ show allValid

  -- -- print $ "obstList=" ++ show obstList

  -- let obsSet = Set.fromList obstList
  -- let cRes = Set.size obsSet
  -- putStrLn $ "Answer 2> " ++ show cRes




  -- let (size, grid) = gridMap
  -- let pathgrid = foldl addPathToMap grid path

  -- print "=== Empty MAP ======="
  -- putStrLn (mapToStr gridMap)
  -- print "=== Path MAP ======="
  -- putStrLn (mapToStr (size, pathgrid))

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type GridMap = GridMapCore Char
type Guard = (Pos, DirVec)

move :: GridMap -> Guard -> Guard
move (size, grid) (pos, dir) = case Map.lookup nextPos grid of
  Just '#' -> (pos, turnRight dir)
  Just 'O' -> (pos, turnRight dir)
  Just '.' -> (nextPos, dir)
  Just '^' -> (nextPos, dir)
  Nothing  -> (nextPos, dir) -- error ("Illegal move p=" ++ show pos ++ ", d=" ++ show dir)
  where
    nextPos = addDir pos dir

turnRight :: DirVec -> DirVec
turnRight (1, 0) = (0, -1)
turnRight (0, -1) = (-1, 0)
turnRight (-1, 0) = (0, 1)
turnRight (0, 1) = (1, 0)

computePath :: GridMap -> [Guard] -> Guard -> [Guard]
computePath gridmap path startGuard
  | elem (nextPos, nextDir) path = path
  | not (isValidPos (fst gridmap) nextPos) = path
  | otherwise = computePath gridmap (newPath nextPos) (nextPos, nextDir)
  where
    (nextPos, nextDir) = move gridmap startGuard
    newPath curPos
      | curPos == fst (head path) = (nextPos, nextDir) : drop 1 path
      | otherwise = (nextPos, nextDir) : path

findStart :: GridMap -> Char -> Pos
findStart (_, gridmap) c = fst $ head $ Map.toList (Map.filter (== c) gridmap)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

detectPathLoop :: GridMap -> [Guard] -> Guard -> Bool
detectPathLoop obstMap path startGuard
  | not (isValidPos (fst obstMap) nextPos) = False
  | elem (nextPos, nextDir) path = True -- `debug` ("FoundLoop \n" ++ show path ++ "\n" ++ (mapToStr (fst obstMap, foldl addPathToMap (snd obstMap) path)))
  | otherwise = detectPathLoop obstMap (newPath nextPos) (nextPos, nextDir)
  where
    (nextPos, nextDir) = move obstMap startGuard
    newPath curPos
      | curPos == fst (head path) = path      -- only (Pos, Dir) before obstacle
      | otherwise = (nextPos, nextDir) : path

-- WARNING => bien faire attention que ajouter un obstacle à un croisement, "plus tard", le long
--         de la trajectoire NE marche PAS !!! (car on l'aurait percuté avant !!)
--         importante de 'visitedPos'
computeLoopPos :: GridMap -> [Pos] -> [Pos] -> Guard -> [Pos]
computeLoopPos gridmap prevObstList visitedPos startGuard
  | not (isValidPos (fst gridmap) nextPos) = prevObstList
  | elem nextPos visitedPos = computeLoopPos gridmap prevObstList (nextDir:visitedPos) (nextPos, nextDir)
  | otherwise = case putObstacle gridmap nextPos of
      (True, obstGridMap) -> if detectPathLoop obstGridMap [startGuard] startGuard
                                then computeLoopPos gridmap (nextPos:prevObstList) (nextPos:visitedPos) (nextPos, nextDir) -- `debug` ("Put obstacle in pos=" ++ show nextPos ++ " (d=" ++ show (snd startGuard) ++ ")" )
                                else computeLoopPos gridmap prevObstList (nextPos:visitedPos) (nextPos, nextDir)
      (False, _)          -> computeLoopPos gridmap prevObstList (nextPos:visitedPos) (nextPos, nextDir)
  where
    (nextPos, nextDir) = move gridmap startGuard

putObstacle :: GridMap -> Pos -> (Bool, GridMap)
putObstacle gridmap pos = case Map.lookup pos (snd gridmap) of
  Just '^' -> (False, gridmap)
  Just '#' -> (False, gridmap)
  Just '.' -> (True, (fst gridmap, Map.adjust (const 'O') pos (snd gridmap)))
  Nothing  -> error ("cannot put obstacle in pos=" ++ show pos)


mapToStr :: GridMap -> String
mapToStr (size, grid) = chunks (snd size) $ map snd (Map.toList grid)

addPathToMap :: Map.Map Pos Char -> (Pos, DirVec) -> Map.Map Pos Char
addPathToMap grid (pos, dir) = Map.adjust (const (dirToStr dir)) pos grid

dirToStr :: DirVec -> Char
dirToStr (-1, 0) = 'N'
dirToStr (0, -1) = 'W'
dirToStr (1, 0) = 'S'
dirToStr (0, 1) = 'E'
