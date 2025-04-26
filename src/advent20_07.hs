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
  putStrLn "** Advent 2020 - Day 07 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input07.txt"
  -- content <- readFile "Input20/test07_1.txt"

  let contRules = map parseBag (lines content)
  -- putStrLn $ "contRules=" ++ show contRules

  let inverted = foldl distributeContained Map.empty contRules
  -- putStrLn $ "inverted=" ++ show inverted

  let containerList = expandContained inverted
                                      Set.empty
                                      (Set.fromList( inverted Map.! "shiny gold" ))
  -- putStrLn $ "containterList=" ++ show containerList

  let pRes = Set.size containerList
  putStrLn $ "Answer 1> " ++ show pRes

  let directRules = map parseBaggage (lines content)
  -- putStrLn $ "directRules=" ++ show directRules

  -- tous les sac, MOINS celui de départ car on veut les sacs contenus
  let cRes = opExpand directRules ("shiny gold", 1) - 1
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

parseBag :: String -> (String, [String])
parseBag str
  | endswith "no other bags." str = (extractColor container, [])
  | otherwise = (extractColor container, containedList)
  where
    (container:contained) = splitOn " contain " str
    containedList = map (extractColor . drop 2) (splitOn ", " $ head contained)

extractColor :: String -> String
extractColor = reverse . tail . dropWhile (/=' ') . reverse

extractNb :: String -> Int
extractNb = digitToInt . head

type InvertedRules = Map.Map String [String]

-- go from [(container, [containedList])] to [(contained, [containerList])]
distributeContained :: InvertedRules -> (String, [String]) -> InvertedRules
distributeContained allRules (outerbag, insides) = foldl addContainer allRules insides
  where
    addContainer rules innerbag = case Map.lookup innerbag rules of
      Nothing -> Map.insert innerbag [outerbag] rules
      Just _ -> Map.adjust ((:) outerbag) innerbag rules

-- expand a list of contained bags to a list of bags that could countain it
expandContained :: InvertedRules -> Set.Set String -> Set.Set String -> Set.Set String
expandContained rules acc setToExpand
  | Set.null setToExpand = acc
  | otherwise = case Map.lookup bag rules of
      Nothing -> expandContained rules accSet (Set.fromList bs)
      Just containers ->expandContained rules accSet (Set.union
                                                         (Set.fromList containers)
                                                         (Set.fromList bs)
                                                        )
      where
        (bag:bs) = Set.toList setToExpand
        accSet = Set.insert bag acc

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
type NbBag = (String, Int)
type Rules = [(String, [NbBag])]

parseBaggage :: String -> (String, [NbBag])
parseBaggage str
  | endswith "no other bags." str = (extractColor container, [])
  | otherwise = (extractColor container, containedList)
  where
    (container:contained) = splitOn " contain " str
    containedList = map (\sbag ->((extractColor . drop 2) sbag, extractNb sbag)) (splitOn ", " $ head contained)

expandBag :: Rules -> String -> Maybe [NbBag]
expandBag rules bag = case lookup bag rules of
  Nothing -> Nothing
  Just [] -> Nothing
  Just bs -> Just bs

opExpand :: Rules -> NbBag -> Int
opExpand rules (bag, nb) = case expandBag rules bag of
  Nothing -> nb
  Just manyBags -> nb + nb * (sum $ map (opExpand rules) manyBags)
