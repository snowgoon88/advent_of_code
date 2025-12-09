{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.Foldable ( foldl' )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List.Extra ( sortOn, splitOn )
-- import Data.Ord ( comparing, Down(..) )

-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)

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
  putStrLn "** Advent 2025 - Day 09 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let pts = map parsePt (lines content)
  let as = reverse ( sortOn fst $ makeArea pts)
  -- putStrLn $ "as=" ++ show as
  -- let pRes = (fst . head) as
  -- putStrLn $ "Answer 1> " ++ show pRes

  let (sndQ, fstH) = sndQuadrant pts
  putStrLn $ "2ndQ=" ++ show sndQ
  let uRect = reverse ( sortOn fst $ upperRect (P 94904 50444) sndQ fstH)
  putStrLn $ "UpperRect=" ++ show (take 3 uRect)
  let uRect2 = reverse ( sortOn fst $ upperRect (P 97603 50471) sndQ fstH)
  putStrLn $ "UpperRect=" ++ show (take 3 uRect2)
  let (trdQ, sndH) = trdQuadrant pts
  let lRect = reverse ( sortOn fst $ lowerRect trdQ sndH)
  putStrLn $ "LowerRect=" ++ show (take 3 lRect)

  print "SMART--------"
  let cRes = take 3 (reverse ( sortOn fst $ pb2 pts))
  putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Pt = P { px::Int, py::Int }
  deriving ( Show )

parsePt line = P (read x) (read y)
  where
    (x:y:_) = splitOn "," line

area :: Pt -> Pt -> Int
area (P x1 y1) (P x2 y2) = (1+abs (x2 - x1)) * (1+abs (y2 - y1))

makeArea :: [Pt] -> [(Int, (Pt, Pt))]
makeArea [] = []
makeArea (p:ps) = map (\v -> (area p v, (p, v))) ps ++ makeArea ps
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- because of the path (see helpers/fig_25_09.png), big rectangles are either
-- above y>= 50444 or below y<= 48302
-- 1708,50444
-- 94904,50444
-- 94904,48302
-- 1837,48302
-- AND one of the corner is 94905,50444 or 94904,48302 (ligne 249)
-- 94905,50444 to one pt in "second" quadrant
-- 94904,48302 to one pt in "third" quadrant

sndQuadrant ps = (dropWhile (\p -> (py p) < maxY) firstHalf, firstHalf)
  where
    firstHalf = takeWhile (\p -> (py p) >= 50444) ps
    maxY = maximum (map py firstHalf)

trdQuadrant ps = (takeWhile (\p -> (py p) >= minY) secondHalf, secondHalf)
  where
    secondHalf = dropWhile (\p -> (py p) >= 48302) ps
    minY = minimum (map py secondHalf)

isInArea :: Pt -> Pt -> Pt -> Bool
isInArea c1 c2 p = (px p) > xmin && (px p) < xmax && (py p) > ymin && (py p) < ymax
  where
    xmax = max (px c1) (px c2)
    xmin = min (px c1) (px c2)
    ymax = max (py c1) (py c2)
    ymin = min (py c1) (py c2)
upperRect op ps toCheck = map (\v -> (area v op, (v, op))) validCorners
  where
    validCorners = filter (valid op) ps
    valid p1 p2 = not (any (isInArea p1 p2) toCheck)

lowerRect ps toCheck = map (\v -> (area v (P 94904 48302), (v, P 94904 48302))) validCorners
  where
    validCorners = filter (valid (P 94904 48302)) ps
    valid p1 p2 = not (any (isInArea p1 p2) toCheck)

-- SMART no intersection rect/Perimeter
intersects :: Pt -> Pt -> [Pt] -> Bool
intersects (P x y) (P x' y') = not . all away . pairs
  where
    -- pairs of successiv pts of the perimeters
    pairs (p : ps) = zip (p : ps) (ps ++ [p])

    --
    away (P lx ly, P lx' ly') =
      -- perim line is on the left
      (max lx lx' <= min x x')
        -- perim line is on the right
        || (min lx lx' >= max x x')
        -- perim line is is below
        || (max ly ly' <= min y y')
        -- perim lin is above
        || (min ly ly' >= max y y')


-- pb2 :: [Pt] -> Int
pb2 poly =
    [ (area p p', (p, p'))
    | p <- poly,
      p' <- poly,
      not (intersects p p' poly)
    ]
