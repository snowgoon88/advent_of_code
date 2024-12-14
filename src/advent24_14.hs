{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
-- import Data.List ( sort, groupBy, group, sortOn ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust, catMaybes, isNothing )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
-- import qualified Data.Set as Set
import Data.List ( delete, sortOn ) -- delete, sortOn
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
  putStrLn "** Advent 2024 - Day 14 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input14.txt"
  let sizeMap = (101, 103)
  -- content <- readFile "Input24/test14_1.txt"
  -- let sizeMap = (11, 7)

  let robStart = map parseRobot (lines content)
  -- print $ "robStart=" ++ show robStart

  let robFinal = map (moveRob sizeMap 100) robStart
  let robQuad = map (toQuadrant sizeMap) robFinal
  let robGather = gatherRob robQuad
  -- print "======="
  -- print $ "robFinal=" ++ show robFinal
  -- print $ "robQuad=" ++ show robQuad
  -- print $ "robGather=" ++ show robGather

  let pRes = product robGather
  putStrLn $ "Answer 1> " ++ show pRes

  -- let memPos = map (loopRobot Map.empty 0 sizeMap) robStart
  -- let period = map (\(first, second, mem) -> (first, second, Map.size mem)) memPos
  -- -- print $ "memPos=" ++ show memPos
  -- print $ period=" ++ show period

  -- let allCloseInit = allClose robStart
  -- print $ "allCloseInit=" ++ show allCloseInit

  -- let lClose = loopTest [] (10403,0) sizeMap robStart
  -- print $ "lClose=" ++ show lClose



  -- let whichClose = any fst lClose
  -- print $ "whichClose=" ++ show whichClose

  let baseMap = (sizeMap, Map.fromList [((x,y), '.') | x <- [0..fst sizeMap], y <- [0..snd sizeMap]])


  -- let best = head $ sortOn fst lClose
  -- print $ "best=" ++ show best

  -- Symetry => no !
  -- let (when, res, newRobots) = loopFind (10403, 0) sizeMap robStart hasSymetry
  -- print $ "==> it=" ++ show when ++ " res=" ++ show res

  let allInt = loopApply (10403, 0) [] sizeMap robStart distToCenter
  let (it, dist, robs) = head $ sortOn (\(w, d, robots) -> d) allInt
  print $ "bestDist=" ++ show (it, dist)

  putStrLn (niceRobot baseMap robs)





  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Robot = Ro { rpos :: (Int, Int)
                , rvit :: (Int, Int) } deriving Show


parsePair :: String -> (Int, Int)
parsePair str = (read ( head tok ), read (tok !! 1))
  where tok = split "," str

parseRobot :: String -> Robot
parseRobot line = Ro p v
  where
    tok = split " " line
    p = parsePair (drop 2 (head tok))
    v = parsePair (drop 2 (tok !! 1))

moveRob :: (Int, Int) -> Int -> Robot -> Robot
moveRob (sizex, sizey) dt (Ro (x,y) (vx,vy)) = Ro (nx,ny) (vx, vy)
  where
    nx = mod (x + dt * vx) sizex
    ny = mod (y + dt * vy) sizey

-- Quadrant 0 is the middle Lines, then a number for each quadrant
toQuadrant :: (Int, Int) -> Robot -> Int
toQuadrant (sizex, sizey) (Ro (x,y) (_,_))
  | x == middlex || y == middley = 0
  | x < middlex && y < middley = 1
  | x < middlex && y > middley = 2
  | x > middlex && y < middley = 3
  | otherwise = 4
  where
    middlex = (div sizex 2)
    middley = (div sizey 2)

gatherRob :: [Int] -> [Int]
gatherRob lPos = map (\q -> length $ filter (== q) lPos) [1..4]
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
type GridMap = GridMapCore Char
type MemPos = Map.Map Pos Int

loopRobot :: MemPos -> Int -> (Int, Int) -> Robot -> (Int, Int, MemPos)
loopRobot memPos nbMove size robot = case Map.lookup (rpos newRob) memPos of
  Nothing -> loopRobot (Map.insert (rpos newRob) (nbMove+1) memPos) (nbMove+1) size newRob
  Just n  -> (n, nbMove+1, memPos)
  where
    newRob = moveRob size 1 robot

loopFind :: (Int, Int) -> Size -> [Robot] -> (Size -> [Robot] -> Bool)-> (Int, Bool, [Robot])
loopFind (maxIt, curIt) size allRobot func
  | curIt == maxIt = (maxIt, False, allRobot)
  | func size allRobot = (curIt, True, allRobot)
  | otherwise = loopFind (maxIt, curIt+1) size (map (moveRob size 1) allRobot) func


loopApply :: (Int, Int) -> [(Int, Int, [Robot])] -> Size -> [Robot] -> (Size -> [Robot] -> Int)-> [(Int, Int, [Robot])]
loopApply (maxIt, curIt) acc size allRobot func
  | curIt == maxIt = acc
  | otherwise = loopApply (maxIt, curIt+1) ((curIt, func size allRobot, allRobot):acc) size (map (moveRob size 1) allRobot) func

allClose :: [Robot] -> Int
allClose allRob = minimum (map (\pos -> distToAnother pos (delete pos allPos)) allPos)
  where
    allPos = map rpos allRob

hasSymetry :: Size -> [Robot] -> Bool
hasSymetry size allRob = and (map (\pos -> hasSymetric pos (delete pos allPos)) allPos)
  where
    allPos = map rpos allRob
    hasSymetric :: Pos -> [Pos] -> Bool
    hasSymetric (x,y) listPos = elem (y,x) listPos


distToCenter :: Size -> [Robot] -> Int
distToCenter size allRob = sum (map (\r -> abs (fst (rpos r)- middlex) + abs (snd (rpos r) - middley )) allRob)
  where
    middlex = div (fst size) 2
    middley = div (snd size) 2

distToAnother :: Pos -> [Pos] -> Int
distToAnother (x, y) lPos = minimum $ map (\(xo,yo) -> (xo-x)*(xo-x)+(yo-y)*(yo-y)) lPos

closeToAnother :: Pos -> [Pos] -> Bool
closeToAnother _ [] = False
closeToAnother (x, y) ((xo, yo):ps)
  | (xo-x)*(xo-x)+(yo-y)*(yo-y) <= 4 = True
  | otherwise = closeToAnother (x,y) ps

loopTest :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> [Robot] -> [(Int, Int)]
loopTest acc (maxIt, curIt) size allRob
  | curIt < maxIt = loopTest ((allClose nextAllRob, curIt):acc) (maxIt, curIt+1) size nextAllRob
  | otherwise = acc
  where
    nextAllRob = map (moveRob size 1) allRob

niceRobot :: GridMap -> [Robot] -> String
niceRobot gridMap allRob = mapToStr (addRobotStr gridMap allRob)

addRobotStr :: GridMap -> [Robot] -> GridMap
addRobotStr gridMap [] = gridMap
addRobotStr (size, grid) (r:rs) = addRobotStr (size, newGrid) rs
  where
    newGrid = Map.adjust (const '*') (rpos r) grid

mapToStr :: GridMap -> String
mapToStr (size, grid) = chunks (snd size) $ map snd (Map.toList grid)



-- distToOther (x,y) lPos = minimum $ map (\(x,y) (xo,yo) -> (xo-x)*(xo-x) + (yo-y)*(yo-y)) (delete (x,y) lPos)

-- Map a Robot positions ?

area n = (n * (n+1)) - n
perim n = 2*n
