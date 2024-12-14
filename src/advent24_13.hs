{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
import Data.List ( sort, groupBy, group, sortOn ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust, catMaybes, isNothing )
-- import Control.Monad ( fold )
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
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
  putStrLn "** Advent 2024 - Day 13 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input13.txt"
  -- content <- readFile "Input24/test13_1.txt"

  -- regroup lines separated by [""]
  let contLines = filter (/= [""]) $ groupBy (\x y -> x /= "" && y /= "") (lines content)
  -- print contLines

  let machines = map parseMachine contLines
  -- print $ "machines=" ++ show machines

  let sol = map solveMachine machines
  -- print $ "sol=" ++ show sol

  let pSol = map bestSol sol
  -- print $ "pSol=" ++ show pSol

  let pRes = sum pSol
  putStrLn $ "Answer 1> " ++ show pRes

  -- let exact = map exactMachine machines
  -- print $ "exact=" ++ show exact

  let exactAdv = map exactMachine (map transform machines)
  -- print $ "exactAdv=" ++ show exactAdv

  let cRes = sum $ map costSol (concat exactAdv)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Machine = Machine { ma :: (Int, Int)
                       , mb :: (Int, Int)
                       , mp :: (Int, Int)
                       } deriving Show

parseDrop :: String -> Int -> (Int, Int)
parseDrop line nbDrop = (read (drop 2 $ head tok), read (drop 2 (tok !! 1)))
  where
    coordStr = drop nbDrop line
    tok = split ", " coordStr

parseMachine :: [String] -> Machine
parseMachine lines = Machine buttonA buttonB prize
  where
    buttonA = parseDrop (head lines) 10
    buttonB = parseDrop (lines !! 1) 10
    prize   = parseDrop (lines !! 2) 7

isSolution :: Machine -> (Int, Int) -> Bool
isSolution (Machine (xa,ya) (xb,yb) (xp,yp)) (a,b) = (xa*a + xb*b == xp) && (ya*a + yb*b == yp)

solveMachine :: Machine -> [(Int, Int)]
solveMachine machine = filter (isSolution machine) [(a,b) | a <- [0..100], b <- [0..100]]

costSol :: (Int, Int) -> Int
costSol (a,b) = 3*a + b

bestSol :: [(Int, Int)] -> Int
bestSol [] = 0
bestSol listSol = costSol $ (head (sortOn costSol listSol))

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- solving equation
-- a = (xb Y - yb X) / (xb ya - yb xa) => see if division

transform :: Machine -> Machine
transform (Machine (xa,ya) (xb,yb) (xp,yp)) = Machine (xa, ya) (xb, yb) (10000000000000 + xp, 10000000000000 + yp)


exactMachine :: Machine -> [(Int, Int)]
exactMachine (Machine (xa,ya) (xb,yb) (xp,yp))
  | mod numerator denom == 0 = if (mod (xp - xa * aval) xb) == 0
                                  then [(aval , div (xp - xa * aval) xb)]
                                  else []
  | otherwise = []
  where
    numerator = (xb * yp - yb * xp)
    denom = (xb * ya - yb * xa)
    aval = div numerator denom
