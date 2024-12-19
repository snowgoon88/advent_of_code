{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
-- import Data.List ( find, sortOn, groupBy ) -- sortOn, groupBy, find, group, sort, sortBy _)
-- import Data.Tuple ( swap )
import Data.String.Utils ( split, join, startswith) --, startswith, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( delete, sortOn ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
-- import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
import Debug.Trace ( trace ) -- trace :: String > a -> a
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 19 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input19.txt"
  -- content <- readFile "Input24/test19_1.txt"

  let towels = readTowels (head $ lines content)
  let patterns = drop 2 (lines content)
  -- print $ "towels=" ++ show towels

  let matched = map (\p -> dfSearch p(expandSearch towels) isDoneSearch) patterns
  -- print $ "matched=" ++ show matched

  let okPatterns = catMaybes matched

  let pRes = length okPatterns
  putStrLn $ "Answer 1> " ++ show pRes

  -- let c01 = allBFSearch towels (head patterns)
  -- print $ "c01=" ++ show c01

  -- let counts = map (allBFSearch towels) patterns
  -- print $ "counts=" ++ show counts

  -- let init01 = foldl (\m t -> applyTowel m 1 0 (head patterns) t) Map.empty towels
  -- print $ "init01=" ++ show init01

  -- let d01 = develop (head patterns) towels init01 0
  -- print $ "d01=" ++ show d01

  let allDev = map (\p -> develop p towels (initDevelop towels p) 0) patterns
  -- print $ "allDev=" ++ show allDev
  let cRes = sum allDev

  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

readTowels :: String -> [String]
readTowels line = split ", " line

expandSearch :: [String] -> String -> [String]
expandSearch allTowels pat = catMaybes $ map (checkMatch pat) allTowels

isDoneSearch :: String -> Bool
isDoneSearch [] = True
isDoneSearch pat = False

dfSearch :: String -> (String -> [String]) -> (String -> Bool) -> Maybe String
dfSearch patStart expFunc isDoneFunc = loop patStart
  where
    loop x
      | isDoneFunc x = Just x -- find isDone xs
      | otherwise     = case catMaybes $ map loop (expFunc x) of
                          [] -> Nothing
                          (e:es) -> Just e
-- depthFirst :: String -> [String] -> [String] -> Bool
-- depthFirst [] (towel:tsà) _ = True
-- depthFirst pat [] _ = False
-- depthFirst pat (towel:ts) allList = case checkMatch pat towel of
--   Just newPat -> depthFirst newPat allList allList `debug` ("OK tow="++ towel ++", pat=" ++ pat ++ " => " ++ newPat)
--   Nothing -> depthFirst pat ts allList `debug` ("------- pat=" ++ pat ++ ", tow=" ++ towel )

checkMatch :: String -> String -> Maybe String
checkMatch pat towel
  | startswith towel pat = Just (drop (length towel) pat)
  | otherwise = Nothing


-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- -- work only for small patterns
-- allBFSearch :: [String] -> String -> Int
-- allBFSearch towels patStart = loop 0 [patStart]
--   where
--     loop :: Int -> [String] -> Int
--     loop count toExplore = case toExpand of
--       [] -> newCount
--       xs -> loop newCount toExpand -- `debug` ("nc=" ++ show newCount ++ " tE=" ++ show toExpand)
--       where
--         (newCount, toExpand) = countPrune count []
--                                           (concat (map (\p -> map (checkMatch p) towels) toExplore))

-- countPrune :: Int -> [String] -> [Maybe String] -> (Int, [String])
-- countPrune curScore acc [] = (curScore, acc)
-- countPrune curScore acc (pat:ps) = case pat of
--   Just p -> if isDoneSearch p then countPrune (curScore+1) acc ps
--                               else countPrune curScore (p:acc) ps
--   Nothing -> countPrune curScore acc ps

-- Pour chaque fullPattern, on stocke les curseur actuel dans une map, quand on les
-- développe, on update la map (en mergant les COUNT)
type CurMap = Map.Map (Int, String) Int -- (pos in fullPattern, patternLeft) --> count

initDevelop :: [String] ->String -> CurMap
initDevelop towels fullPattern = foldl (\m t -> applyTowel m 1 0 fullPattern t) Map.empty towels

develop :: String -> [String] -> CurMap -> Int -> Int
develop fullPattern towels cursorMap pos
  | pos == length fullPattern = sum ( map (\((p,pat),c) -> c) (cursorsAt cursorMap pos) )
  | otherwise = if nextCursors == []
      then develop fullPattern towels cursorMap (pos+1)
      else develop fullPattern towels newMap (pos+1)
      where
        nextCursors = cursorsAt cursorMap (pos+1)
        newMap = foldl (\cMap ((pos, patLeft), count)
                              --
                              -> foldl (\m t -> applyTowel m count pos patLeft t) cMap towels)
                       cursorMap
                       nextCursors

-- find all (pos, patLeft)->count cursor that start at pos
cursorsAt :: CurMap -> Int -> [((Int, String), Int)]
cursorsAt curMap pos = Map.toList $ Map.filterWithKey (\(p,pat) a -> p == pos) curMap

applyTowel :: CurMap -> Int -> Int -> String -> String -> CurMap
applyTowel curMap count curPos patLeft towel
  | startswith towel patLeft = case Map.lookup (newPos, newPat) curMap of
      Just prevCount -> Map.insert (newPos, newPat) (count+prevCount) curMap
      Nothing -> Map.insert (newPos, newPat) count curMap
  | otherwise = curMap
  where
    newPos = curPos + length towel
    newPat = drop (length towel) patLeft
