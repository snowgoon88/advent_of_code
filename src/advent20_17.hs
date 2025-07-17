{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir,
--                readCoord)
import MyGrid (readCoord)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines
import MyUtils (applyN)
-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Type.Bool as Int
-- import qualified Linear.V2 as LV
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
  putStrLn "** Advent 2020 - Day 17 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input17.txt"
  -- content <- readFile "Input20/test17_1.txt"

  -- 'active' is a list of Pos3D of '#'
  let active = readActive3D (lines content)
  putStrLn $ "active=" ++ show active

  -- let n100 = neighbors (1, 0, 0)
  -- putStrLn $ "n100=" ++ show n100

  -- let aMap01 = addNeighbors Map.empty [(1, 0, 0), (3, 0, 0)]
  -- putStrLn $ "aMap01=" ++ show aMap01

  -- let step01 = stepGameLife (Set.fromList active)
  -- putStrLn $ "step01=" ++ show step01

  let after06 = applyN (stepGameLife neighbors3D) 6 (Set.fromList active)
  -- putStrLn $ "after06=" ++ show after06

  let pRes = Set.size after06
  putStrLn $ "Answer 1> " ++ show pRes

  let active4D = readActive4D (lines content)
  let after4D = applyN (stepGameLife neighbors4D) 6 (Set.fromList active4D)
  let cRes = Set.size after4D
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Pos3D = (Int, Int, Int)
-- all neigbors of a Pos3D
neighbors3D :: Pos3D -> [Pos3D]
neighbors3D (x, y, z) = [(nx, ny, nz) | nx <- [x-1 .. x+1]
                                    , ny <- [y-1 .. y+1]
                                    , nz <- [z-1 .. z+1]
                                    , nx /= x || ny /= y || nz /= z]

{--
On peut faire plus SMART avec des V3 Int ou V4 Int.
> pure [-1,0,1] :: V2 [Int]
V3 [-1,0,1] [-1,0,1]
class Functor f => Applicative f where
  pure :: a -> f a

et donc> sequence ( pure [-1, 0, 1] ) :: [V2 Int]
[V2 (-1) (-1),V2 (-1) 0,V2 (-1) 1,V2 0 (-1),V2 0 0,V2 0 1,V2 1 (-1),V2 1 0,V2 1 1]
car 'sequence' evaluate each monadic action in the structure from left to right.
class (Functor t, Foldable t) => Traversable t
sequence :: Monad m => t (m a) -> m (t a)

ce qui permet de faire neighbors comme un set avec
neighbsSet :: V3 Int -> Set (V3 Int)
neighbsSet p = S.fromList
    [ p + d
    | d <- sequence (pure [-1,0,1])
    , d /= pure 0
    ]

Et en fait, ça marche aussi avec V4, V2, etc...
--}


-- map of number of active neighbors
type ActMap p = Map.Map p Int

readActive3D :: [String] -> [Pos3D]
readActive3D allLines = [(x, y, 0) | (_, (x,y)) <- listRead ]
  where
    (_, listRead) = readCoord "." allLines

--alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
-- actNeigbors aMap actList =

-- compute all the neighbors from the list of active Pos3D
-- addNeighbors :: (Ord p, Foldable t) => ActMap p -> (p -> [p]) -> t p -> ActMap p
addNeighbors :: (Foldable t1, Foldable t2, Ord k, Num a) =>
     Map.Map k a -> (p -> t2 k) -> t1 p -> Map.Map k a
addNeighbors aMap neigF allPos = foldl (opAddAllNeighbors neigF) aMap allPos

-- opAddAllNeighbors :: (Ord p) => (p -> [p]) -> ActMap p -> p -> ActMap p
opAddAllNeighbors :: (Foldable t, Ord k, Num a) =>
     (p -> t k) -> Map.Map k a -> p -> Map.Map k a
opAddAllNeighbors neigF aMap pos = foldl opAddNeighbor aMap (neigF pos)

opAddNeighbor :: (Ord k, Num a) => Map.Map k a -> k -> Map.Map k a
opAddNeighbor aMap pos = case Map.lookup pos aMap of
  Nothing -> Map.insert pos 1 aMap
  Just n -> Map.insert pos (n+1) aMap


-- one step:
-- 1: all active pos => map of number of active neighbors
-- 2: aMap => newActive <- pos with 3 active neighbors
-- 3: add all active with 2 or 3 active neighbors
setWithNeighbors :: Eq a => Map.Map k a -> a -> Set.Set k
setWithNeighbors aMap  n = Map.keysSet $ Map.filter (== n) aMap

stillAct :: (Ord a1, Num a2, Eq a2) => Map.Map a1 a2 -> Set.Set a1 -> Set.Set a1
stillAct aMap actSet = Set.intersection actSet (setWithNeighbors aMap 2)

stepGameLife :: (Ord p) => (p -> [p]) -> Set.Set p -> Set.Set p
stepGameLife neigF actSet = Set.union newActSet stillActSet
  where
    actMap = addNeighbors Map.empty neigF (Set.elems actSet)
    newActSet = setWithNeighbors actMap 3
    stillActSet = stillAct actMap actSet

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
type Pos4D = (Int, Int, Int, Int)
-- all neigbors of a Pos4D
neighbors4D :: Pos4D -> [Pos4D]
neighbors4D (x, y, z, w) = [(nx, ny, nz, nw) | nx <- [x-1 .. x+1]
                                    , ny <- [y-1 .. y+1]
                                    , nz <- [z-1 .. z+1]
                                    , nw <- [w-1 .. w+1]
                                    , nx /= x || ny /= y || nz /= z || nw /= w]
readActive4D :: [String] -> [Pos4D]
readActive4D allLines = [(x, y, 0, 0) | (_, (x,y)) <- listRead ]
  where
    (_, listRead) = readCoord "." allLines
