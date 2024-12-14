{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
import Data.List ( sort, group, sortOn, groupBy ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
-- import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Data.List ( sortOn )
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
  putStrLn "** Advent 2024 - Day 08 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input08.txt"
  -- content <- readFile "Input24/test08_1.txt"


  let sizeGrid = (length (lines content), length (head (lines content)))
  print $ "sizeGrid=" ++ show sizeGrid

  let antennae = concat (map (\il -> parseLine [] (fst il) (zip (snd il) [0..]))
                          (zip [0..] (lines content)))
  -- print $ "antennae" ++ show antennae

  let sortA = sort antennae
  -- print $ "sortA=" ++ show sortA
  let groupedA = groupBy (\a1 a2 -> fst a1 == fst a2) sortA
  -- print $ "groupedA=" ++ show groupedA

  let nodes = antennaeToNodes sizeGrid groupedA
  -- print $ "nodes=" ++ show nodes

  let finaleNodes = Set.fromList (concat nodes)
  let pRes = Set.size finaleNodes
  putStrLn $ "Answer 1> " ++ show pRes

  let resos = antennaeToReso sizeGrid groupedA
  -- print $ "nodes=" ++ show nodes

  let finaleReso = Set.fromList (concat resos)
  let cRes = Set.size finaleReso
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Pos = (Int, Int)
type Size = (Int, Int)

-- inside SIze
isValidPos :: Size -> Pos -> Bool
isValidPos (rMax, cMax) (r, c) = r >= 0 && c >= 0 && r < rMax && c < cMax


parseLine :: [(Char, Pos)] -> Int -> [(Char, Int)] -> [(Char, Pos)]
parseLine soFar row (('.', col):zs) = parseLine soFar row zs
parseLine soFar row ((c, col):zs) = parseLine ((c, (row, col)):soFar) row zs
parseLine soFar _ [] = soFar

computeNode :: Pos -> Pos -> Pos
computeNode (r1, c1) (r2, c2) = (r1 + 2 * (r2 - r1), c1 + 2 * (c2 - c1))

antennaeToNodes :: Size -> [[(Char, Pos)]] -> [[Pos]]
antennaeToNodes size groupedAntennae = map ((allNodes size) . (map snd )) groupedAntennae

allNodes :: Size -> [Pos] -> [Pos]
allNodes size posL = filter (isValidPos size) [computeNode pa pb | pa <- posL, pb <- posL, pb /= pa]

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

computeAligned :: Size -> Pos -> Pos -> [Pos]
computeAligned size p1 p2
  | isValidPos size p2 = p2 : computeAligned size p2 (computeNode p1 p2)
  | otherwise = []

allResonance :: Size -> [Pos] -> [Pos]
allResonance size posL = concat [computeAligned size  pa pb | pa <- posL, pb <- posL, pb /= pa]

antennaeToReso :: Size -> [[(Char, Pos)]] -> [[Pos]]
antennaeToReso size groupedAntennae = map ((allResonance size) . (map snd )) groupedAntennae
