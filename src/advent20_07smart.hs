{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
import Data.Char (digitToInt)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Set as Set
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
-- ****** Data.List.Extra: splitOn
import Data.List.Extra (splitOn, delete)
import Data.List.Utils (endswith)
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

i1 :: String
i1 = "light red bags contain 1 bright white bag, 2 muted yellow bags."

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 07 Part - & -                                            **"
  putStrLn "********************************************************************************"
  -- content <- readFile "Input20/input07.txt"
  content <- readFile "Input20/test07_1.txt"

  let grDirect = foldl parseBaggage Map.empty (lines content)
  putStrLn $ "** grDirect *****\n" ++ showGraph grDirect showContained

  let grAscend = inverseToAscend grDirect
  putStrLn $ "** grAscend *****\n" ++ showGraph grAscend showParents

  let grInverse = inverse grDirect
  putStrLn $ "** grInverse *****\n" ++ showGraph grInverse showContained

  let contained = descendant grInverse "shiny gold"
  putStrLn $ "contained=" ++ show contained

  let containedSmart = (descendantSmart grInverse) Map.! "shiny gold"
  putStrLn $ "containedSmart=" ++ show contained

  let pRes = length contained
  putStrLn $ "Answer 1> " ++ show pRes

  let nbB = nbBags grDirect Map.! "shiny gold"
  putStrLn $ "nbBags=" ++ show nbB
  let nbSmart = nbBagsSmarts grDirect Map.! "shiny gold"
  putStrLn $ "nbBagsSmart=" ++ show nbSmart
  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type NbBag = (String, Int)
type ValueGraph k v = Map.Map k (Map.Map k v)
type GColor = ValueGraph String Int

parseBaggage :: GColor -> String -> GColor
parseBaggage gr str
  | endswith "no other bags." str = Map.insert (extractColor container) Map.empty gr
  | otherwise = Map.insert (extractColor container) (Map.fromList containedList) gr
  where
    (container:contained) = splitOn " contain " str
    containedList = map (\sbag ->((extractColor . drop 2) sbag, extractNb sbag)) (splitOn ", " $ head contained)

extractColor :: String -> String
extractColor = reverse . tail . dropWhile (/=' ') . reverse

extractNb :: String -> Int
extractNb = digitToInt . head

-- to display nice graphs
showContained :: (Show k, Show b) => Map.Map k b -> String
showContained cmap
  | Map.null cmap = "-\n"
  | otherwise     = Map.foldlWithKey opShow "" cmap ++ "\n"
  where
    opShow str color value = str ++ show color ++ "(" ++ show value ++ "); "

showGraph :: Show k => Map.Map k b -> (b -> [Char]) -> [Char]
showGraph gr showVal = Map.foldlWithKey opShow "" gr
  where
    opShow str key val = str ++ show key ++ ":-> " ++ showVal val

-- inverse grColor to have a graph of ascendant
-- première idée, créer une carte des ascendants de type
type GAscend = Map.Map String (Set.Set String)

-- union with current Map
inverseToAscend :: GColor -> GAscend
inverseToAscend gr = Map.foldlWithKey opFold Map.empty gr
  where
    opFold :: GAscend -> String -> Map.Map String Int -> GAscend
    opFold acc color contained = Map.unionWith Set.union acc newMap
      where
        -- make Map color (Set String)
        -- from [(contained color1, color), (contained color2, color), ...]
        newMap = Map.fromList $ zip (Map.keys contained) (repeat (Set.singleton color))

-- pour utiliser dans showGraph ensuite
showParents :: Show b => Set.Set b -> String
showParents pset = Set.foldl opShow "" pset ++ "\n"
  where
    opShow str parent = str ++ show parent ++ "; "

-- MAIS, il y a plus simple si on fait la Map des ascendant avec le même type
-- que GColor = Map color (Map ascendant, nb_pouvant_être_contenu)
-- que l'on peut afficher directement avec showGraph gr showContained
inverse :: (Ord k1, Ord k2) =>
     Map.Map k2 (Map.Map k1 a) -> Map.Map k1 (Map.Map k2 a)
inverse gr = Map.fromListWith Map.union
  [ (contained, Map.singleton color nbBag)
  | (color, cmap) <- Map.toList gr
  , (contained, nbBag) <- Map.toList cmap
  ]

-- "travel" graph to find all descendant
-- graph color :-> Set of all descendant

-- TODO if color in descendant, then add the descendant from cmap
-- opDescend grDes color cmap =
descendant :: Ord k => Map.Map k (Map.Map k b) -> k -> Set.Set k
descendant graph key = case Map.lookup key graph of
  Nothing -> Set.empty
  Just childrenMap -> foldl Set.union
                            (Set.fromList (Map.keys childrenMap))
                            (map (descendant graph) (Map.keys childrenMap))

{- -- SMART plus malin avec fmap ? ***************************************
fmap :: Functor f => (a -> b) -> f a -> f b
(Map k) being a Functor, this turns a Map k (Map k v) into a Map k (Set k)

uses
Map.foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
with the Set.Set monoid (produced by Set.insert)

allDescendants :: Ord v => Graph v e -> Map v (Set v)
allDescendants gr = descendantMap
  where
    descendantMap = gr <&>
      M.foldMapWithKey (\v _ -> S.insert v (M.findWithDefault S.empty v descendantMap))

-- note: (<&>) is flip fmap
-}
-- Map.foldMapWithKey (\key val -> )
descendantSmart :: Ord k => Map.Map k (Map.Map k a)
                            -> Map.Map k (Set.Set k)
descendantSmart graph = descendantMap
  where
    -- (Map k) being a Functor, this turns a Map k (Map k v) into a Map k (Set k)
    -- fmap :: Functor f => (a -> b) -> f a -> f b
    descendantMap = fmap (Map.foldMapWithKey (opFoldMap descendantMap)) graph

-- insert a key to the set of its descendant
-- this function produces a Set, which is a monoid
opFoldMap :: Ord k => Map.Map k (Set.Set k) -> k -> p -> Set.Set k
opFoldMap descendantMap key _ = Set.insert key (findChildren key descendantMap)
-- find the set of childredn of key in descendantMap
findChildren :: Ord k => k -> Map.Map k (Set.Set a) -> Set.Set a
findChildren key descendantMap = Map.findWithDefault Set.empty key descendantMap

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- bon, essayons d'être SMART
-- on veut passer de Map k (Map k Int) à Map k (Int)
-- refaire le truc avec fmap
nbBags :: (Num b, Ord k) => Map.Map k (Map.Map k b) -> Map.Map k b
nbBags graph = nbMap
  where
    nbMap = fmap (opContainedToNb nbMap) graph

-- on va utiliser FoldMapWithKey sur (Map color nbContained)
    opContainedToNb sumMap containedMap = sum [ n * (findNbContained k sumMap + 1)
                                             | (k, n) <- Map.toList containedMap]
    findNbContained key bagsMap = Map.findWithDefault 0 key bagsMap

nbBagsSmarts :: (Num b, Ord k) => Map.Map k (Map.Map k b) -> Map.Map k b
nbBagsSmarts gr = usageMap
  where
    usageMap = fmap (\neighbors -> sum
      [ n * (Map.findWithDefault 0 v usageMap + 1)
      | (v, n) <- Map.toList neighbors
      ])
      gr
