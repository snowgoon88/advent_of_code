{-# LANGUAGE TupleSections #-}
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
import Data.String.Utils (startswith)
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl)
-- ****** Data.List.Extra: splitOn
import Data.List.Extra (splitOn, sortOn, delete)
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
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
  putStrLn "** Advent 2020 - Day 16 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input16.txt"
  --content <- readFile "Input20/test16_1.txt"
  --content <- readFile "Input20/test16_2.txt"

  let parts = splitOn "\n\n" content
  -- putStrLn $ "parts=" ++ show parts
  let rules = map parseRule (lines $ head parts)
  putStrLn $ "rules=" ++ show rules

  let myTicket = parseTicket (head (drop 1 $ lines (parts !! 1)))
  -- putStrLn $ "ticket=" ++ show myTicket

  -- otherTickets :: [[Int]]
  let otherTickets = map parseTicket (drop 1 $ lines (parts !! 2))
  -- putStrLn $ "others=" ++ show otherTickets

  -- putStrLn $ "7 is invalid ? " ++ show (invalidVal rules 7)
  -- putStrLn $ "4 is invalid ? " ++ show (invalidVal rules 4)

  let pRes = sum $ filter (invalidVal rules) (concat otherTickets)
  putStrLn $ "Answer 1> " ++ show pRes

  let validTickets = myTicket : filter (not . (invalidTicket rules)) otherTickets
  -- putStrLn $ "validTickets=" ++ show validTickets

  let filtered = filterRules rules validTickets
  -- putStrLn $ "filtered=" ++ show filtered
  let ordered = orderByLen filtered
  -- putStrLn $ "length=" ++ show ordered

  let clean1 = removeRule (head (snd $ head ordered)) (tail ordered)
  -- putStrLn $ "clean1=" ++ show clean1

  let cleaned = cleanRules filtered
  putStrLn $ "cleaned=" ++ show cleaned

  let extract = fieldValue myTicket cleaned
  putStrLn $ "extact=" ++ show extract

  let cRes = product (map (\(v,_,_) -> v)  extract)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Ticket = [Int]
data Rule = R { rlabel :: String
              , rrange1 :: (Int, Int)
              , rrange2 :: (Int, Int) }
  deriving (Show)
-- Eq to ensure Rules can be deleted from List
instance Eq Rule where
  r1 == r2 = rlabel r1 == rlabel r2

parseRule :: String -> Rule
parseRule line = R label (rleft !! 0, rleft !! 1) (rright !! 0, rright !! 1)
  where
    label = takeWhile (/= ':') line
    [r1, _, r2] = words (drop (length label + 2) line)
    rleft = map read $ splitOn "-" r1
    rright = map read $ splitOn "-" r2

parseTicket :: String -> Ticket
parseTicket line = map read $ splitOn "," line

-- validFor | is val ok with rule ?
validFor :: Int -> Rule -> Bool
validFor val (R _ (minL, maxL) (minR, maxR)) = val >= minL && val <= maxL ||
  val >= minR && val <= maxR

invalidVal :: Foldable t => t Rule -> Int -> Bool
invalidVal rules val = all (not . (validFor val)) rules

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
invalidTicket :: (Foldable t1, Foldable t2) => t2 Rule -> t1 Int -> Bool
invalidTicket rules ticket = any (invalidVal rules) ticket

-- must have something like pos (Int) -> rule

-- given a list of rules and a val, remove all invalid rules
-- listRules -> val -> listCompatibleRules
keepRules :: [Rule] -> Int -> [Rule]
keepRules rules val = filter (validFor val) rules

-- build a list of allRules (one for each position),
-- then use every ticket to remove invalid rules at every position
-- list of (position, compatible rules)
filterRules :: Foldable t => [Rule] -> t [Int] -> [(Int, [Rule])]
filterRules rules tickets = zip [1..] $ foldl opKeep allRules tickets
  where
    allRules = take (length rules) (repeat rules)
opKeep listRules ticket = map (\(rules, val) -> keepRules rules val) $ zip listRules ticket

-- build Pos -> ONE compatible rule by starting with the most constrained
-- position (i.e. only one rule)
orderByLen :: [(Int, [Rule])] -> [(Int, [Rule])]
orderByLen rules = sortOn (length . snd) rules

-- recursively remove "lonely" rules from the other lists of candidates.
removeRule :: Rule -> [(Int, [Rule])] -> [(Int, [Rule])]
removeRule rule ((pos, rules):rs) = (pos, delete rule rules) : removeRule rule rs
removeRule _ [] = []

cleanRules :: [(Int, [Rule])] -> [(Int, [Rule])]
cleanRules [] = []
cleanRules rules = (pos, rule) : cleanRules removed
  where
    ((pos, rule):rs) = sortOn (length. snd) rules
    removed = removeRule (head rule) rs

-- seek for "departure xxx" on tickets
fieldValue ticket rules = foldl opTicket [] rules
 where
   opTicket :: [(Int, Int, String)] -> (Int, [Rule]) -> [(Int, Int, String)]
   opTicket acc (pos, rule) = if startswith "departure" (rlabel $ head rule)
     then (ticket !! (pos-1), pos, rlabel $ head rule) : acc
     else acc
