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
import Data.String.Utils (split)
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
-- ****** Data.List.Extra: splitOn
import Data.List (elemIndices)
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits
import qualified System.Random.Stateful as R

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

randGen :: R.StdGen
randGen = R.mkStdGen 1234

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2021 - Day 07 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let crabL = parseCrab content
  let median = quickSelect (div (length crabL) 2) crabL
  putStrLn $ "median=" ++ show median

  let pRes = sum $ map (crabDist median) crabL
  putStrLn $ "Answer 1> " ++ show pRes

  let mean = div (sum crabL) (length crabL)
  putStrLn $ "mean=" ++ show mean
  let dist = allAritDist crabL
  -- putStrLn $ "dist=" ++ show dist
  let cRes = minimum dist
  let pivotBest = head $ elemIndices (minimum dist) dist
  putStrLn $ "Answer 2> " ++ show cRes ++ " at=" ++ show pivotBest

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
parseCrab :: String -> [Int]
parseCrab line = map read (split "," line)

-- quickSelect using first element as pivot
-- TODO: need to use Random selection of Pivot
quickSelect _ []   = error ("Quickselect out")
quickSelect _ [x]  = x
quickSelect k (x:xs)
  | k == lenBefore + 1 = x
  | (lenBefore + 1) < k  = quickSelect (k - lenBefore -1) listAfter
  | otherwise      = quickSelect k listBefore
  where
    ((lenBefore, listBefore), (_, listAfter)) = partitionCount (< x) (xs)


-- partition a list in two, countLeadingZeros
partitionCount :: (Foldable t, Num a1, Num a2)
  => (a3 -> Bool)     -- function to select yes/no members
  -> t a3             -- members
  -> ((a1, [a3]), (a2, [a3])) -- (nb_of_yes, yes_list) (nb_of_no, no_list)
partitionCount f ls = foldr opSelect ((0,[]), (0,[])) ls
  where
    opSelect n ((cy, ly), (cn, ln))
      | f n = ((cy+1, n:ly), (cn, ln))
      |otherwise = ((cy, ly), (cn+1, n:ln))

crabDist :: Num a => a -> a -> a
crabDist n1 n2 = abs (n2 - n1)

-- randomly chose a pivot
-- uniformR include low and upper bounds
randomIntM n = R.uniformRM (0, n)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- distance as arithmetic sum
aritDist n1 n2 = div (n*(n+1)) 2
  where n = crabDist n1 n2

allAritDist l = map (\n -> sum (map (aritDist n) l)) [minimum l .. maximum l]
