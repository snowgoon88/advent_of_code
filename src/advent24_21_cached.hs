{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
import Data.String.Utils ( split, join, startswith) --, startswith, join, replace, split )
-- import qualified Data.Map.Strict as Map
import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( sort, group, find, delete, sortOn, groupBy ) -- delete, sortOn
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
  putStrLn "** Advent 2024 - Day 21 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input21.txt"
  -- content <- readFile "Input24/test21_1.txt"

  -- putStrLn $ "Answer 1> " ++ show pRes

  -- ([(Int, Int)], CacheMap) -> String -> ([(Int, Int)], CacheMap)
  let opComplex (acc, cache) code = ((len, read (init code)):acc, newCache)
        where (len, newCache) = cachedComplexDP cache 26 code

  let results = foldl opComplex ([], Map.empty) (lines content)
  print $ "res=" ++ show (fst results)

  let cRes = sum $ map (\(l,v) -> v*l) (fst results)
  putStrLn $ "Answer 2> " ++ show cRes


  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Pos = (Int, Int)
type ResMap = Map.Map Pos String

hamming :: Pos -> Pos -> Int
hamming (r1,c1) (r2,c2) = abs (r1-r2) + abs (c1-c2)

posD :: Char -> Pos
posD 'A' = (0, 2)
posD '0' = (0, 1)
posD c = (row+1,col)
  where (row,col) = divMod (digitToInt c - 1) 3

isValidD :: Pos -> Bool
isValidD (row,col) = (row >=0) && (row <= 3) == (col >= 0) && (col <= 2) && ((row,col) /= (0, 0))

posA :: Char -> Pos
posA 'A' = (1, 2)
posA '^' = (1, 1)
posA '<' = (0, 0)
posA 'v' = (0, 1)
posA '>' = (0, 2)

isValidA :: Pos -> Bool
isValidA (row,col) = (row >=0) && (row <= 1) == (col >= 0) && (col <= 2) && ((row,col) /= (1, 0))

move :: Pos -> Char -> Pos
move (row,col) '^' = (row+1, col)
move (row,col) '<' = (row, col-1)
move (row,col) 'v' = (row-1, col)
move (row,col) '>' = (row, col+1)

-- using DynamicProgramming on
-- State = (fromPos, toPos, level_rob) => nb steps of the best option
type State = (Char, Char, Int, Int)
type CacheMap = Map.Map State Int

-- robot 0 is in front of the door
minMoves :: State -> Int
minMoves (from, to, level, maxRobot)
  -- for HUMAN : nbArrow cells
  | level == (maxRobot - 1) = hamming (pos from) (pos to) + 1 -- nbArrow cells + 'A'
  | otherwise = minimum allValues
  where
    pos = if level == 0 then posD else posA
    isValid = if level == 0 then isValidD else isValidA
    -- allSequences are of the form "", "^^>", "<<", etc
    allSequences = map (\(reached, path) -> "A"++path++"A") (allPath (pos from) (pos to) isValid)
    allValues = map value allSequences
    value :: String -> Int
    value [c] = 0
    value (c1:c2:cs) = minMoves (c1, c2, level+1, maxRobot) + value (c2:cs)

complexDP :: Int -> String -> Int
complexDP nbRobot code = opComplex ("A"++code)
  where
    opComplex :: String -> Int
    opComplex [c] = 0
    opComplex (c1:c2:cs) = minMoves (c1, c2, 0, nbRobot) + opComplex (c2:cs)

cachedMinMove :: CacheMap -> State -> (Int, CacheMap)
cachedMinMove  cacheMap (from, to, level, maxRobot)
  | level == (maxRobot - 1) = (hamming (pos from) (pos to) + 1, cacheMap) -- nbArrow cells + 'A'
  | otherwise = allValues
  where
    pos = if level == 0 then posD else posA
    isValid = if level == 0 then isValidD else isValidA
    -- allSequences are of the form "", "^^>", "<<", etc
    allSequences = map snd (allPath (pos from) (pos to) isValid)

    allValues :: (Int, CacheMap)
    allValues = (minimum values, newCache)
       where (values, newCache) = foldl opValue ([], cacheMap) allSequences

    opValue :: ([Int], CacheMap) -> String -> ([Int], CacheMap)
    opValue (acc, cache) seq = (val:acc, newCache)
      where (val, newCache) = value cache seq

    value :: CacheMap -> String -> (Int, CacheMap)
    value cache seq = (sum values, valuedCache)
      where (values, valuedCache) = foldl opCached ([], cache) (zip ("A"++seq) (seq++"A"))

    opCached :: ([Int], CacheMap) -> (Char, Char) -> ([Int], CacheMap)
    opCached (acc,cache) (c1,c2) = case Map.lookup (c1, c2, level+1, maxRobot) cache of
      Just val -> (val:acc, cache)
      Nothing ->  (val:acc, updatedCache)
        where
          (val, newCache) = cachedMinMove cache (c1, c2, level+1, maxRobot)
          updatedCache = Map.insert (c1, c2, level+1, maxRobot) val newCache

cachedComplexDP :: CacheMap -> Int -> String -> (Int, CacheMap)
cachedComplexDP cacheMap nbRobot code = (sum moves, updatedCache)
  where
    (moves, updatedCache) = foldl opSum ([],cacheMap) (zip ("A" ++ init code) code)

    opSum :: ([Int], CacheMap) -> (Char, Char) -> ([Int], CacheMap)
    opSum (acc, cache) (c1, c2) = (nbMove:acc, newCache)
      where (nbMove, newCache) = cachedMinMove cache (c1, c2, 0, nbRobot)

expandPos :: (Int, Int) -> Pos -> Pos -> (Pos -> Bool) -> [(Pos, String)] -> [(Pos, String)]
expandPos (maxLevel, level) start goal isValid toExplore
  | level == maxLevel = filter (\(pos,path) -> pos == goal) toExplore
  | otherwise = expandPos (maxLevel, level+1) start goal isValid newExplore
  where
    reachable = concat $ map neighbors toExplore
      where
        neighbors :: (Pos, String) -> [(Pos, String)]
        neighbors (pos, path) = map (\d -> (move pos d, path ++ [d])) "^<v>"
    valids = filter (\(p,c) -> isValid p) reachable
    newExplore = filter (\(p,c) -> hamming p start == level) valids

-- reachables :: [(Pos, String)] -> [(Pos, String)]
-- reachables pospaths = concat $ map neighbors pospaths
-- neighbors :: (Pos, String) -> [(Pos, String)]
-- neighbors (pos, path) = map (\d -> (move pos d, path ++ [d])) "^<v>"
-- valids :: (Pos -> Bool) -> [(Pos, String)] -> [(Pos, String)]
-- valids isValid pospaths = filter (\(p,c) -> isValid p) pospaths
-- explorables :: Int -> Pos -> [(Pos, String)] -> [(Pos, String)]
-- explorables level start pospaths = filter (\(pos, path) -> hamming pos start == level) pospaths

allPath :: Pos -> Pos -> (Pos -> Bool) -> [(Pos, String)]
allPath start goal isValid = expandPos (1 + hamming start goal, 1) start goal isValid [(start,"")]

pathDigipad :: Char -> String -> [String]
pathDigipad start [c] = map (++ "A") paths
  where
    paths = map snd (allPath (posD start) (posD c) isValidD)
pathDigipad start (c:cs) = [p1 ++ p2 | p1 <- pathDigipad start [c], p2 <- pathDigipad c cs]

pathArrow :: Char -> String -> [String]
pathArrow start [c] = map (++ "A") paths
  where
    paths = map snd (allPath (posA start) (posA c) isValidA)
pathArrow start (c:cs) = [p1 ++ p2 | p1 <- pathArrow start [c], p2 <- pathArrow c cs]

doorRobot :: String -> [String]
doorRobot code = pathDigipad 'A' code

coldRobot :: String -> [String]
coldRobot code = filter (\s -> length s == minLength) allPaths
  where
    allPaths = concat $ map (pathArrow 'A') (doorRobot code)
    minLength = minimum (map length allPaths)

myHand :: String -> [String]
myHand code = filter (\s -> length s == minLength) allPaths
  where
    allPaths = concat $ map (pathArrow 'A') (coldRobot code)
    minLength = minimum (map length allPaths)

-- handPath :: String -> String
-- handPath code = pathArrowpad 'A' (pathArrowpad 'A' (pathDigipad 'A' code))

complexity :: String -> (Int, Int)
complexity code = (length goodPath, read (init code))
  where
    goodPath = head (myHand code)


-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- Digit : priorité terminer par ^ ou >, OU v, (pas <)
-- maximiser fleches pareilles
--
-- 029A :    <   A ^ A ^^  > A vvv   A
--          v <<   A >>^A<A>A<AAv>A^A<AAA>^A
--        v<A<AA<<^AvAA
myHandOpti "029A" = pathDigiOPti 'A' (pathDigiOPti 'A' "<A^A^^>AvvvA")
myHandOpti "980A" = pathDigiOPti 'A' (pathDigiOPti 'A' "^^^A<AvvvA>A")
myHandOpti "179A" = pathDigiOPti 'A' (pathDigiOPti 'A' "^<<A^^A>>AvvvA")
myHandOpti "456A" = pathDigiOPti 'A' (pathDigiOPti 'A' "^^<<A>A>AvvA")
myHandOpti "379A" = pathDigiOPti 'A' (pathDigiOPti 'A' "^A<<^^A>>AvvvA")

myHandOpti "836A" = pathDigiOPti 'A' (pathDigiOPti 'A' "<^^^Avv>A^AvvA")
myHandOpti "540A" = pathDigiOPti 'A' (pathDigiOPti 'A' "<^^A<A>vvA>A")
myHandOpti "965A" = pathDigiOPti 'A' (pathDigiOPti 'A' "^^^AvA<Avv>A")
myHandOpti "480A" = pathDigiOPti 'A' (pathDigiOPti 'A' "^^<<A^>AvvvA>A")
myHandOpti "789A" = pathDigiOPti 'A' (pathDigiOPti 'A' "^^^<<A>A>AvvvA")


handPattern "029A" =  "<A^A^^>AvvvA"
handPattern "980A" =  "^^^A<AvvvA>A"
handPattern "179A" =  "^<<A^^A>>AvvvA"
handPattern "456A" =  "^^<<A>A>AvvA"
handPattern "379A" =  "^A<<^^A>>AvvvA"

handPattern "836A" =  "<^^^Avv>A^AvvA"
handPattern "540A" =  "<^^A<A>vvA>A"
handPattern "965A" =  "^^^AvA<Avv>A"
handPattern "480A" =  "^^<<A^>AvvvA>A"
handPattern "789A" =  "^^^<<A>A>AvvvA"
-- CANNOT COMPUTE
totalPath "836A" = foldl (\path c -> pathDigiOPti 'A' path) "<^^^Avv>A^AvvA" [0..25]

complexityOpti :: String -> (Int, Int)
complexityOpti code = (length goodPath, read (init code))
  where
    goodPath = myHandOpti code

complexityPattern :: String -> (Int, Int)
complexityPattern code = (lengthPattern patterns, read (init code))
  where
    patterns = patternBot $ patternBot $ patternBot (digitize (handPattern code))

complexityLong :: Int -> String -> (Int, Int)
complexityLong nbBot code = (lengthPattern patterns, read (init code))
  where
    patterns = foldl opBot (digitize (handPattern code)) [0..(nbBot+1)]

-- Pattenrns are xxxA
-- Store Pattenrs
type PatMap = Map.Map String [(String, Int)] -- Pattern -> [(Count, Pattern)]

digitize :: String -> [(String, Int)]
digitize str = map (\g -> (head g ++ "A", length g)) grouped
  where grouped = group $ sort $ init (split "A" str)

expandPattern :: (String, Int) -> [(String, Int)]
expandPattern (pat,count) = map (\(p,nb) -> (p, nb*count)) $ digitize $ pathDigiOPti 'A' pat

gatherPattern :: [(String, Int)] -> [(String, Int)]
gatherPattern patterns = map (\g -> (fst $ head g, sum (map snd g))) grouped
  where
    grouped = groupBy (\pc1 pc2 -> fst pc1 == fst pc2) $ sort $ patterns

patternBot :: [(String, Int)] -> [(String, Int)]
patternBot patterns = gatherPattern $ concat (map expandPattern patterns)

opBot :: [(String, Int)] -> Int -> [(String, Int)]
opBot patterns dummy = patternBot patterns

lengthPattern :: [(String, Int)] -> Int
lengthPattern patterns = sum (map snd patterns)

-- from Patterns to all next Patterns
nextPatterns :: (String, Int) -> [[(String, Int)]]
nextPatterns (pat, count) = map gatherPattern countedPat
  where
    allNext = map digitize (pathArrow 'A' pat)
    countedPat = map (map (\(p,c) -> (p,c*count))) allNext

firstPatterns :: (String, Int) -> [[(String, Int)]]
firstPatterns (pat, count) = map gatherPattern countedPat
  where
    allNext = map digitize (pathDigipad 'A' pat)
    countedPat = map (map (\(p,c) -> (p,c*count))) allNext

expandPatterns :: [(String, Int)] -> [[(String, Int)]]
expandPatterns [pat] = nextPatterns pat
expandPatterns (pat:ps) = [gatherPattern (p1 ++ p2) | p1 <- nextPatterns pat,
                         p2 <- expandPatterns ps]

opPatternBot :: [[(String, Int)]] -> Int -> [[(String,Int)]]
opPatternBot patterns dummy = filter (\p -> lengthPattern p == minLength) allPatterns
  where
    allPatterns = concat $ map expandPatterns patterns
    minLength = minimum (map lengthPattern allPatterns)

complexPattern :: Int -> String -> (Int, Int)
complexPattern nbRob code = (lengthPattern goodPattern, read (init code))
  where
    doorPatterns = firstPatterns (code, 1)
    allPatterns = foldl opPatternBot doorPatterns [0..nbRob]
    goodPattern = head allPatterns


--gather pattenrs =
-- digitizeRobot patterns =
--   where
--     expanded (nb, pat) = map (\(c, pat))
-- *****************************************************************************

pathDigiOPti :: Char -> String -> String
pathDigiOPti start [c] = digipath start c
pathDigiOPti start (c:cs) = pathDigiOPti start [c] ++ pathDigiOPti c cs

digipath 'A' 'A' = "A"
digipath '<' 'A' = ">>^A"
digipath '^' 'A' = ">A"
digipath 'v' 'A' = "^>A"
digipath '>' 'A' = "^A"

digipath 'A' '<' = "v<<A"
digipath '<' '<' = "A"
digipath '^' '<' = "v<A"
digipath 'v' '<' = "<A"
digipath '>' '<' = "<<A"

digipath 'A' '^' = "<A"
digipath '<' '^' = ">^A"
digipath '^' '^' = "A"
digipath 'v' '^' = "^A"
digipath '>' '^' = "<^A"

digipath 'A' 'v' = "<vA"
digipath '<' 'v' = ">A"
digipath '^' 'v' = "vA"
digipath 'v' 'v' = "A"
digipath '>' 'v' = "<A"

digipath 'A' '>' = "vA"
digipath '<' '>' = ">>A"
digipath '^' '>' = "v>A"
digipath 'v' '>' = ">A"
digipath '>' '>' = "A"

doorRobotDebug :: String -> [String]
doorRobotDebug code = pathDigipad 'A' code

coldRobotDebug :: String -> [(String, String)]
coldRobotDebug code = filter (\(path,_) -> length path == minLength) allPaths
  where
    allPaths = concat $ map (\path -> map (,path) (pathArrow 'A' path)) (doorRobotDebug code)
    minLength = minimum (map (\(p,_) -> length p) allPaths)

myHandDebug :: String -> [(String, String, String)]
myHandDebug code = filter (\(path, _, _) -> length path == minLength) allPaths
  where
    allPaths = concat $ map (\(path,origin) -> map (,path,origin) (pathArrow 'A' path)) (coldRobotDebug code)
    minLength = minimum (map (\(path, _, _) -> length path) allPaths)
