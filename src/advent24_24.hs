{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC

import Data.String.Utils ( split, join, startswith) --, startswith, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( find, sortOn, groupBy ) -- sort, group, find, delete, sortOn, groupBy ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 24 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input24.txt"
  -- content <- readFile "Input24/test24_1.txt"
  -- content <- readFile "Input24/test24_2.txt"

  -- regroup lines separated by [""]
  let contLines = filter (/= [""]) $ groupBy (\x y -> x /= "" && y /= "") (lines content)
  let gates = foldl parseGate Map.empty (head contLines)
  let (pMap, procs) = foldl parseProcessor (Map.empty, []) (contLines !! 1)
  -- print $ "gates=" ++ show gates
  -- print $ "procs=" ++ show procs

  -- OK
  let rel = relax gates procs
  -- print $ "rel=" ++ show rel

  let oKeys = getOutputKeys rel
  let oVal = getOutput rel
  -- print $ "oKeys=" ++ show oKeys
  -- print $ "oVal=" ++ show oVal
  -- print (niceGates rel)

  let pRes = toDecimal 1 oVal
  putStrLn $ "Answer 1> " ++ show pRes

  -- -- GraphViz
  -- putStrLn "digraph Advent24_24 {"
  -- putStrLn $ unlines (toViz gates procs)
  -- putStrLn "}"

  print "Teste tous les digit un par un pour trouver les mauvais"
  mapM_ (\d -> print $ testDigit procs d) [0..44]

  print "et repare à la main avec Graphviz"
  let cRes = "dvb,fhg,fsq,tnc,vcf,z10,z17,z39"
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Cmd = AND | OR | XOR
  deriving (Eq, Ord, Show)

data Processor = Pro { p_i1 :: String
                     , p_i2 :: String
                     , p_op :: Cmd
                     , p_out :: String }
    deriving Show

type GateMap = Map.Map String Int
type OutMap = Map.Map String Processor

parseGate :: GateMap -> String -> GateMap
parseGate gMap line = Map.insert (head tok) (read $ tok !! 1) gMap
  where
    tok = split ":" line

parseProcessor :: (OutMap, [Processor]) -> String -> (OutMap, [Processor])
parseProcessor (gMap, ps) line = (Map.insert (tok !! 4) newP gMap, newP:ps)
  where
    tok = split " " line
    op = case (tok !! 1) of
      "AND" -> AND
      "OR" -> OR
      "XOR" -> XOR
    newP = Pro (head tok) (tok !! 2) op (tok !! 4)

process :: GateMap -> Processor -> Maybe (Int, GateMap)
process gMap (Pro in1 in2 cmd out)
  | isReady gMap in1 && isReady gMap in2 = Just (val, newGMap)
  | otherwise = Nothing
  where
    val = compute cmd (gMap Map.! in1) (gMap Map.! in2)
    newGMap = Map.insert out val gMap

isReady :: GateMap -> String -> Bool
isReady gMap name = case Map.lookup name gMap of
  Nothing -> False
  Just v -> True


compute :: Cmd -> Int -> Int -> Int
compute AND v1 v2 = v1 * v2
compute OR v1 v2 = max v1 v2
compute XOR v1 v2 = Bits.xor v1 v2 -- mod (v1+v2) 1


-- recProcess (gMap, outMap) name
--   | isReady gMap name = gMap Map.! name
--   | otherwise = compute cmd v1 v2
--   where
--     pro = outMap

opProcess :: (GateMap, [Processor], [Processor]) -> [Processor] -> (GateMap, [Processor], [Processor])
opProcess (gMap, procWait, procOK) [] = (gMap, procWait, procOK)
opProcess (gMap, procWait, procOK) (pro:ps) = case process gMap pro of
  Nothing -> opProcess (gMap, pro:procWait, procOK) ps
  Just (v, newGMap) -> opProcess (newGMap, procWait, pro:procOK) ps

relax :: GateMap -> [Processor] -> GateMap
relax gMap [] = gMap
relax gMap procList = relax newGMap toProcess
  where
    (newGMap, toProcess, _) = opProcess (gMap, [], []) procList

getOutputKeys :: GateMap -> [String]
getOutputKeys gMap = filter (startswith "z") (Map.keys gMap)

getOutput :: GateMap -> [Int]
getOutput gMap = map (gMap Map.!) (getOutputKeys gMap)

toDecimal :: Int -> [Int] -> Int
toDecimal power [d] = power * d
toDecimal power (d:ds) = d * power + toDecimal (power*2) ds

niceGates gMap = map (\(s,v) -> s ++ ": " ++ show v) (Map.toList gMap)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

proViz (Pro in1 in2 cmd out)
  | cmd == AND = "node [shape=box]; " ++ out
  | cmd == OR = "node [shape=diamond]; " ++ out
  | cmd == XOR = "node [shape=triangle]; " ++ out

nodViz name = "node [shape=circle]; " ++ name

edgeViz (Pro in1 in2 cmd out) = [in1 ++ " -> " ++ out ++";", in2 ++ " -> " ++ out ++ ";"]

toViz gMap procList = map nodViz (Map.keys gMap) ++ map proViz procList ++ concat (map edgeViz procList)

-- test each digit
genZeroGMap = Map.fromList (concat [[("x"++toNb x, 0), ("y"++toNb x, 0)] | x <- [0..44]])
genGMap gMap d = [insert gMap x y d | x <- [0,1], y <- [0,1]]
  where
    insert gm x y d = Map.insert ("y"++toNb d) y (Map.insert ("x"++toNb d) x gm)

testDigit procList d = (d, map (\g -> getNiceOut (relax g procList) [d,d+1]) (genGMap genZeroGMap d))

getNiceOut gMap listD = map (\d -> gMap Map.! ("z"++toNb d)) listD

toNb :: Int -> String
toNb d
  | d < 10 = "0"++show d
  |otherwise = show d
