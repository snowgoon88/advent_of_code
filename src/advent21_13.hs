{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import qualified MyGrid as MG
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines
import qualified MyUtils as MU
-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List.Extra (splitOn)
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)
import GHC.Float (expts)

-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug :: c -> String -> c
-- debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2021 - Day 13 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let (pointLines:foldLines:_) = MU.groupLines (lines content)
  let pts = Set.fromList $ map readPoint pointLines
  -- putStrLn $ "pts=" ++ show pts

  let splits = map readSplit foldLines
  -- putStrLn $ "splits=" ++ show splits

  let (_, pt01) = apply (splits !! 0) $ (plotSize pts, pts)
  let pRes = Set.size pt01
  putStrLn $ "Answer 1> " ++ show pRes

  putStrLn $ "sizeInit=" ++ show (plotSize pts)
  let (sizeFinal, ptFinal) = foldr apply (plotSize pts, pts) (reverse splits)
  putStrLn $ "sizeFinal=" ++ show sizeFinal
  -- putStrLn $ "ptFinal=" ++ show ptFinal
  putStrLn $ displayPoints sizeFinal ptFinal
  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Point = LV.V2 Int
type PointSet = Set.Set Point
type Size = (Int, Int)

readPoint :: String -> Point
readPoint line = LV.V2 x y
  where
    (strX:strY:_) = splitOn "," line
    x = read strX
    y = read strY

data Split = HSplit Int | VSplit Int
  deriving (Show)

readSplit :: String -> Split
readSplit line
  | axis == "x" = VSplit (read val)
  | axis == "y" = HSplit (read val)
  | otherwise = error ("readSplit error with [" ++ drop 11 line ++ "]")
  where
    (axis:val:_) = splitOn "=" (drop 11 line)

apply :: Split -> (Size, PointSet) -> (Size, PointSet)
apply (HSplit y) ((sx, _), ptSet) = ((sx, y-1), Set.union keep moved)
  where
    (keep, move) = Set.partition (\(LV.V2 _ py) -> py < y) ptSet
    moved = Set.map (\(LV.V2 px py) -> LV.V2 px (2*y - py)) move
apply (VSplit x) ((_, sy), ptSet) = ((x-1, sy), Set.union keep moved)
  where
    (keep, move) = Set.partition (\(LV.V2 px _) -> px < x) ptSet
    moved = Set.map (\(LV.V2 px py) -> LV.V2 (2*x - px) py) move

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- size of Plot
plotSize :: PointSet -> (Int, Int)
plotSize ptSet = (maxX, maxY)
  where
    maxX = maximum (Set.map (\(LV.V2 px _) -> px) ptSet)
    maxY = maximum (Set.map (\(LV.V2 _ py) -> py) ptSet)

-- nice plot pts, BEWARE LV.V2 are listed in order (0, 0), (0, 1), (0, 2), (1, 0)
-- which is not the order needed for ploting
-- (0, 1), (1, 0), (2, 0)
displayPoints :: Size -> PointSet -> String
displayPoints (sx, sy) ptSet = MG.chunks (sx+1) plotElems
  where
    -- maxX = maximum (Set.map (\(LV.V2 px _) -> px) ptSet)
    -- maxY = maximum (Set.map (\(LV.V2 _ py) -> py) ptSet)
    wholeGrid = Map.fromList [(LV.V2 px py, '.') | px <- [0..sx], py <- [0..sy]]
    dottedGrid = Map.unionWith (\_ _ -> '#') wholeGrid (Map.fromSet (const '#') ptSet)
    plotElems = [(\ (x, y) -> dottedGrid Map.! LV.V2 x y) (px, py) |
                   py <- [0 .. sy], px <- [0 .. sx]]
