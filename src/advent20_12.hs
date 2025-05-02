{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
import Data.Maybe ( mapMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
import Linear (V2(..), (*^))
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
-- ****** Data.List.Extra: splitOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
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
  putStrLn "** Advent 2020 - Day 12 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input12.txt"
  -- content <- readFile "Input20/test12_1.txt"

  let inst = mapMaybe parseCmd (lines content)
  let res1 = run (V2 0 0, E) inst
  putStrLn $ "res1=" ++ show res1
  let pRes = sum (abs (fst res1))
  putStrLn $ "Answer 1> " ++ show pRes

  let res2 = runWay (V2 0 0, V2 10 1) inst
  putStrLn $ "res2=" ++ show res2
  let cRes = sum (abs (fst res2))
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Vec = V2 Int

-- directions
data Dir = N | W | S | E
  deriving (Eq, Enum, Bounded, Show)

dir2Vec :: Dir -> Vec
dir2Vec N = V2 0 1
dir2Vec W = V2 (-1) 0
dir2Vec S = V2 0 (-1)
dir2Vec E = V2 1 0

data Move = F | L | R
  deriving (Eq, Enum, Bounded, Show)

type Pose = (Vec, Dir)   -- pos, dir
data Cmd = Abs Dir Int | Rel Move Int   -- ('N', 10), etc
  deriving (Show)

-- | Circular version of 'succ'
csucc :: (Eq a, Enum a, Bounded a) => a -> a
csucc x | x == maxBound = minBound
        | otherwise     = succ x

-- | Circular version of 'pred'
cpred :: (Eq a, Enum a, Bounded a) => a -> a
cpred x | x == minBound = maxBound
        | otherwise     = pred x

parseCmd :: String -> Maybe Cmd
parseCmd ('N':cs) = Just $ Abs N (read cs)
parseCmd ('W':cs) = Just $ Abs W (read cs)
parseCmd ('S':cs) = Just $ Abs S (read cs)
parseCmd ('E':cs) = Just $ Abs E (read cs)
parseCmd ('F':cs) = Just $ Rel F (read cs)
parseCmd ('R':cs) = Just $ Rel R (div (read cs) 90)
parseCmd ('L':cs) = Just $ Rel L (div (read cs) 90)
parseCmd _ = Nothing

step :: Pose -> Cmd -> Pose
step (pos, dir) (Abs d val) = (pos + (fmap (* val) $ dir2Vec d), dir)
step (pos, dir) (Rel F val) = (pos + (fmap (* val) $ dir2Vec dir), dir)
step (pos, dir) (Rel L val) = (pos, iterate csucc dir !! val)
step (pos, dir) (Rel R val) = (pos, iterate cpred dir !! val)

run :: Foldable t => Pose -> t Cmd -> Pose
run pose instructions = foldl step pose instructions

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- PoseWay (Position, Waypoint)
type PoseWay = (Vec, Vec)

rotWayPtR :: Vec -> Vec
rotWayPtR (V2 x y) = (V2 y (-x))
rotWayPtL :: Vec -> Vec
rotWayPtL (V2 x y) = (V2 (-y) x)

stepWay :: PoseWay -> Cmd -> PoseWay
-- stepWay (pos, way) (Abs d val) = (pos, way + (fmap (* val) $ dir2Vec d))
stepWay (pos, way) (Abs d val) = (pos, way + val *^ (dir2Vec d))
stepWay (pos, way) (Rel L val) = (pos, iterate rotWayPtL way !! val)
stepWay (pos, way) (Rel R val) = (pos, iterate rotWayPtR way !! val)
stepWay (pos, way) (Rel F val) = (pos + (fmap (* val) $ way), way)

runWay poseWay instructions = foldl stepWay poseWay instructions
