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
-- import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( find, sortOn ) -- sort, group, find, delete, sortOn, groupBy ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
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
  putStrLn "** Advent 2024 - Day 23 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input23.txt"
  -- content <- readFile "Input24/test23_1.txt"

  let neighbors = foldl parseLine Map.empty (lines content)
  -- print $ "neigbors=" ++ show neighbors

  -- let nAQ = subGraph neighbors "aq"
  -- print $ "nAQ=" ++ show nAQ

  -- let okGraphAQ = map validGraph nAQ
  -- print $ "okGraphAQ=" ++ show okGraphAQ

  let allSub = concat $ map (subGraph neighbors) (Map.keys neighbors)
  let valids = filter validGraph allSub
  -- print $ "valids=" ++ show valids

  let pRes = length valids
  putStrLn $ "Answer 1> " ++ show pRes

  let allN = foldl parseLineAll Map.empty (lines content)
  -- print $ "allN=" ++ show allN
  -- mapM_ (\n -> print $ n ++ " -> " ++ niceClique (allN Map.! n)) (Map.keys allN)

  let allClique = algoBR allN Set.empty (Set.fromList (Map.keys allN)) Set.empty []
  -- print $ "allClique=" ++ show allClique
  -- mapM_ (print . niceClique) allClique

  let cRes = findMaxClique allClique
  putStrLn $ "Answer 2> " ++ show cRes
  print (niceClique (snd cRes))

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Neigbors = Map.Map String [String]

parseLine :: Neigbors -> String -> Neigbors
parseLine nMap line
  | n1 < n2 = case Map.lookup n1 nMap of
      Nothing -> Map.insert n1 [n2] nMap
      Just l -> Map.insert n1 (Set.toList (Set.fromList (n2:l))) nMap
  | n2 < n1 = case Map.lookup n2 nMap of
      Nothing -> Map.insert n2 [n1] nMap
      Just l -> Map.insert n2 (Set.toList (Set.fromList (n1:l))) nMap
  | otherwise = nMap --connected to itself
  where
    tok = split "-" line
    n1 = head tok
    n2 = tok !! 1

subGraph :: Neigbors -> String -> [[String]]
subGraph nMap node = case Map.lookup node nMap of
  Nothing -> []
  Just ln -> [n2:n1:[node] | n1 <- ln, n2 <- ln, n1 < n2, connected nMap n1 n2]

connected :: Neigbors -> String -> String -> Bool
connected nMap n1 n2
  | n1 < n2 = areConnected n1 n2
  | n2 < n1 = areConnected n2 n1
  | otherwise = error ("should not test n n")
  where
    areConnected n1 n2 = case Map.lookup n1 nMap of
      Nothing -> False
      Just ns -> elem n2 ns

validGraph :: [String] -> Bool
validGraph ns = any (startswith "t") ns
-- subGraph nMap ns = map grow ns
--   where
--     grow graph = [n ++ graph | n <- addNeigbors nMap (head graph)]

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
type Nodes = Set.Set String
type NeiMap = Map.Map String Nodes

parseLineAll :: NeiMap -> String -> NeiMap
parseLineAll nMap line = add (add nMap n1 n2) n2 n1
  where
    tok = split "-" line
    n1 = head tok
    n2 = tok !! 1
    add nodeMap node1 node2 = case Map.lookup node1 nMap of
      Nothing -> Map.insert node1 (Set.singleton node2) nodeMap
      Just ns -> Map.insert node1 (Set.insert node2 ns) nodeMap

-- BronKerbosh2 https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
-- clique include all nodes of RS
-- clique include some node of PS
-- clique include none of XS
-- PS and XS are disjoint, mais PS U XS sont joints à RS
--
-- for each v of PS, v is added to RS, PS et XS restreints aux voisins de v
-- => 'report' (ie accumulate) toutes les cliques trouvées
algoBR :: NeiMap -> Nodes -> Nodes -> Nodes -> [Nodes] -> [Nodes]
algoBR nMap rs ps xs acc
  | Set.null ps && Set.null xs = rs:acc
  | otherwise = newACC
  where
    -- pivot : point avec le plus de voisins
    --         comme on va tester ses voisins, u sera testé aussi
    nMaxNeig = nodeWithMaxNeigbors nMap (Set.union ps xs)
    (newRS, newPS, newXS, newACC) = foldl (opBR nMap) (rs, ps, xs, acc)
                                          (Set.difference ps (nMap Map.! nMaxNeig))

opBR :: NeiMap -> (Nodes, Nodes, Nodes, [Nodes]) -> String -> (Nodes, Nodes, Nodes, [Nodes])
opBR nMap (rs, ps, xs, acc) v = (rs, Set.delete v ps, Set.insert v xs, gs)
  where
    gs = algoBR nMap (Set.insert v rs)
                     (Set.intersection ps (nMap Map.! v))
                     (Set.intersection xs (nMap Map.! v))
                     acc

nodeWithMaxNeigbors :: NeiMap -> Nodes -> String
nodeWithMaxNeigbors nMap ns = case lookup lenMax lenNodes of
      Just p -> p
      Nothing -> error ("max exists ln=" ++ show lenNodes ++ ", lenMax=" ++ show lenMax)
  where
    lenNodes = map (\n -> (Set.size (nMap Map.! n), n)) (Set.toList ns)
    lenMax = maximum (map fst lenNodes)

niceClique :: Nodes -> String
niceClique ns = join "," (Set.toList ns)

findMaxClique :: [Nodes] -> (Int, Nodes)
findMaxClique ns = last sorted
  where
    lenNodes = map (\n -> (Set.size n, n)) ns
    sorted = sortOn fst lenNodes
