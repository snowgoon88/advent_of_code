{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import Data.List ( sort, group, sortOn, groupBy ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
import Debug.Trace ( trace ) -- trace :: String > a -> a
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug = flip trace


l01 = "75,47,61,53,29"
l02 = "97,61,53,29,13"
l03 = "75,29,13"
l04 = "75,97,47,61,53"

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 05 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input05.txt"
  -- content <- readFile "Input24/test05_1.txt"

  let contLines = groupBy (\x y -> x /= "" && y /= "") (lines content)
  -- print $ "contLines=" ++ show contLines


  let succList = map toPrecMap (gatherSucc $ map parseSucc (head contLines))
  let precMap = Map.fromList succList
  -- print $ "succList=" ++ show succList
  -- print $ "precMap=" ++ show precMap

  let leaflets = map parseLeaflet (head (drop 2 contLines ))
  -- print $ "leaflets=" ++ show leaflets

  let okLeaflets = filter (checkLeaflet precMap) leaflets
  -- print $ "okLeaflets=" ++ show okLeaflets

  -- let pl01 = parseLeaflet l01
  -- print $ "pl01=" ++ show pl01
  -- let ok01 = checkLeaflet precMap [] pl01
  -- print $ "ok01=" ++ show ok01

  let pRes = sum (map getMiddle okLeaflets)
  putStrLn $ "Answer 1> " ++ show pRes

  print "============================"
  let badLeaflets = filter (not . checkLeaflet precMap) leaflets
  -- print $ "badLeaflets=" ++ show badLeaflets

  let repared = map (repare precMap []) badLeaflets
  -- print $ "repared=" ++ show repared

  let cRes = sum (map getMiddle repared)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

-- Map Nb -> Nb that cannot be before (ex 47 -> 53, 13, 29 in ex)
type PrecMap = Map.Map Int [Int]

parseSucc :: String -> (Int, Int)
parseSucc line = (read $ tokens !! 1, read $ head tokens)
  where
    tokens = split "|" line

gatherSucc :: [(Int, Int)] -> [[(Int, Int)]]
gatherSucc lsucc = groupBy (\a b -> fst a == fst b) $ sortOn fst lsucc

toPrecMap :: [(Int, Int)] -> (Int, [Int])
toPrecMap lsucc = (fst $ head lsucc, map snd lsucc)

parseLeaflet :: String -> [Int]
parseLeaflet line = map read (split "," line)

checkNotInSucc :: [Int] -> [Int] -> Bool
checkNotInSucc succList forbidden = not $ or (map (\s -> elem s forbidden) succList)

checkLeaflet :: PrecMap -> [Int] -> Bool
checkLeaflet _ [] = True
checkLeaflet precMap (l:ls)
  | checkNotInSucc ls forbidden = checkLeaflet precMap ls
  | otherwise = False -- `debug` ("fail at l=" ++ show l ++ " succ=" ++ show ls ++ " forb=" ++ show forbidden)
  where
    forbidden = case Map.lookup l precMap of
      Just l -> l
      Nothing -> []

getMiddle :: [Int] -> Int
getMiddle l = l !! (div (length l) 2)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

splitOnforbidden :: [Int] -> [Int] -> [Int] -> (Int, [Int])
splitOnforbidden prec [] forbidden = error ("splitOnForbidden not found in prec=" ++ show prec)
splitOnforbidden prec (suc:ss) forbidden
  | elem suc forbidden = (suc, prec ++ ss)
  | otherwise = splitOnforbidden (prec ++ [suc]) ss forbidden


repare :: PrecMap -> [Int] -> [Int] -> [Int]
repare succMap prec [] = prec
repare succMap prec (l:ls)
  | checkNotInSucc ls forbidden = repare succMap (prec ++ [l]) ls
  | otherwise =  repare succMap prec (repare succMap [] ((fst splitted):l:(snd splitted)))
  where
    splitted = splitOnforbidden [] ls forbidden
    forbidden = case Map.lookup l succMap of
      Just l -> l
      Nothing -> []
