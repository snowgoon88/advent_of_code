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
import qualified MyUtils as MU

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
import Data.Maybe (mapMaybe)
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List.Extra (sort, splitOn)
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)

-- *********************************************************************************** DEBUG
import Debug.Trace ( trace ) -- trace :: String > a -> a
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug :: c -> String -> c
debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

fineDigits :: [String]
fineDigits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf",
              "abdfg", "abdefg", "acf", "abcdef", "abcdfg"]
normalDigits :: [(String, Char)]
normalDigits = [("abcefg", '0'), ("cf", '1'), ("acdeg", '2'), ("acdfg", '3'), ("bcdf", '4'),
                ("abdfg", '5'), ("abdefg", '6'), ("acf", '7'), ("abcdefg", '8'), ("abcdfg", '9')]

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2021 - Day 08 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let allDisplay = map parseDisplay (lines content)
  -- putStrLn $ "fisrtD=" ++ show (head allDisplay)
  let countUnique = map ((MU.countTrue isUnique) . digits) allDisplay
  let pRes = sum countUnique
  putStrLn $ "Answer 1> " ++ show pRes

  -- let mappingFirst = identifySegments (patterns $ head allDisplay)
  -- putStrLn $ "mappFirst=" ++ show mappingFirst
  -- let decoded = decodeNumber mappingFirst (digits $ head allDisplay)
  -- putStrLn $ "decoded=" ++ decoded
  let goodDisplay = map decodeDisplay allDisplay
  -- putStrLn $ "good=" ++ show goodDisplay
  let cRes = sum goodDisplay
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Display = D { patterns :: [String]  -- all 10 unique patterns
                 , digits :: [String] }  -- digits outputs
  deriving (Show)

parseDisplay :: String -> Display
parseDisplay line = D (words (head tok)) (words (tok !! 1))
  where
    tok = splitOn " | " line

-- one of the unique digit (i.e. 1, 4, 7, 8)
isUnique :: String -> Bool
isUnique digit = l == 2 || l == 3 || l == 4 || l == 7
  where l = length digit
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************'
{- Deducing digit segments
   2 : acdeg
   3 : acdfg
   5 : abdfg
   => COMMON is ADG, DIFF is BCEF

   0 : abcefg
   6 : abdefg
   9 : abcdfg
   => COMMON is ABFG, DIFF is CDE

   7 : acf, 1 : cf, 4 : bcdf, 8 : abcdefg

   => A is 7 - 1
     (=> CF is COMMON 17)
   => E is COMMON (DIFF 069 - COMMON 17, DIFF 235)
   => F is 2 - COMMON (235 - (PATTERN 235 with E)) - COMMON 235
   => C is DIFF 235 - E - F
   => D is DIFF 069 - C - E
   => B is 4 - C - D - F
   => G is 8 - ABCDEF
-}
type Mapping = [(Char, Char)]
-- remove s2 from s1
minus :: String -> String -> String
minus s1 s2 = filter (`notElem` s2) s1

-- keep digit(s) that have this segment
withSegment :: [String] -> Char -> [String]
withSegment ds seg = filter (elem seg) ds

-- common segments of ds digits, segments that are not common
commonDiff :: [String] -> (String, String)
commonDiff ds = (commons, diffs)
  where
    nbForCommon = length ds
    -- a list of (seg, 1)
    allSegs = map (, 1) (concat ds) :: [(Char, Int)]
    mapSegs = Map.fromListWith (+) allSegs :: Map.Map Char Int
    commons = Map.keys (Map.filter (== nbForCommon) mapSegs)
    diffs = minus (Map.keys mapSegs) commons

identifySegments :: [String] -> Mapping
identifySegments ds = [(segA, 'a'), (segB, 'b'), (segC, 'c'),
                       (segD, 'd'), (segE, 'e'), (segF, 'f'), (segG, 'g')]
  where
    one = head $ filter (\d -> 2 == length d ) ds
    four = head $ filter (\d -> 4 == length d) ds
    seven = head $ filter (\d -> 3 == length d ) ds
    set235 = filter (\d -> 5 == length d) ds
    set069 = filter (\d -> 6 == length d) ds
    (c17, _) = commonDiff [one, seven]
    (c235, d235) = commonDiff set235
    (_, d069) = commonDiff set069

    segA = head $ minus seven one
    (segEList, _) = commonDiff [minus d069 c17, d235]
    segE = head segEList :: Char
    (cF, _) = commonDiff (filter (segE `notElem`) set235)
    segF = head $ minus cF c235
    (cC, _) = commonDiff [d235, d069]
    segC = head $ minus cC [segE]
    segD = head $ minus d069 [segC, segE]
    segB = head $ minus four [segC, segD, segF]
    segG = head $ minus c235 [segA, segD]

decodeDigit :: Mapping -> String -> Maybe Char
decodeDigit mapping d = lookup mapped normalDigits
  where
    mapped = sort $ map opMap d
    opMap c = case lookup c mapping of
      Just seg -> seg
      Nothing -> error ("Segment -" ++ show c ++ "- not found" ++ show mapping ++ "\n" ++ show d)

decodeNumber :: Mapping -> [String] -> String
decodeNumber mapping ds  = mapMaybe (decodeDigit mapping) ds

decodeDisplay :: Display -> Int
decodeDisplay disp = read $ decodeNumber mapping (digits disp)
  where
    mapping = identifySegments (patterns disp)

--common :: [String] -> String
