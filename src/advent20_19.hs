{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines
import MyUtils (countTrue)

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
import Data.String.Utils (split, startswith) --, replace)
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
-- ****** Data.List.Extra: splitOn
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

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 19 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input19.txt"
  -- content <- readFile "Input20/test19_1.txt"
  -- content <- readFile "Input20/test19_2.txt"

  let parts = split "\n\n" content
  let rulesRead = map readRule (lines $ head parts)
  -- putStrLn $ "rulesRead=" ++ show rulesRead

  -- let pat4 = genPatFromBody rulesRead EndPattern (Pat "a")
  -- putStrLn $ "pat5=" ++ show pat4
  -- let pat5 = genPatFromBody rulesRead EndPattern (Pat "b")
  -- putStrLn $ "pat5=" ++ show pat5
  -- let pat3 = genPatFromBody rulesRead EndPattern (IdR [[4,5],[5,4]])
  -- putStrLn $ "pat3=" ++ show pat3
  let Just body0 = lookup 0 rulesRead
  let pat0 = genPatFromBody rulesRead EndPattern body0 -- (IdR [[4,1,5]])
  -- putStrLn $ "pat0=" ++ show pat0

  let matchCode str = putStrLn $ show (length str) ++ ": " ++ str ++ " -> " ++ show (matchPattern str pat0)
  mapM_ matchCode (lines (head ( tail parts )))

  let pRes = countTrue (`matchPattern` pat0) (lines (head (tail parts)))
  putStrLn $ "Answer 1> " ++ show pRes

  let Just body31 = lookup 31 rulesRead
  let pat31 = genPatFromBody rulesRead EndPattern body31
  putStrLn $ "pat31=" ++ show pat31
  putStrLn $ ">>>>>> " ++ show (allMatch pat31)
  let Just body42 = lookup 42 rulesRead
  let pat42 = genPatFromBody rulesRead EndPattern body42
  putStrLn $ "pat42=" ++ show pat42
  putStrLn $ ">>>>>> " ++ show (allMatch pat42)

  let matchAdv str = putStrLn $ show (length str) ++ ": " ++ str ++ " -> " ++ show (recurMatch 8 pat42 pat31 str)
  mapM_ matchAdv (lines (head ( tail parts )))

  let cRes = countTrue (recurMatch 8 pat42 pat31) (lines (head (tail parts)))
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data RuleBody = IdR [[Int]] | Pat String
 deriving Show

readRule :: String -> (Int, RuleBody)
readRule line = (MP.val idP, pat)
  where
    idP = MP.getID MP.Parser {MP.val=0, MP.toParse=line, MP.stopC=":"} line
    dLine = drop 1 (MP.toParse idP)
    pat = case readMaybe dLine of
      Just str -> Pat str
      Nothing -> IdR (map (map read . words) tok)
    tok = split " | " dLine

-- Map vers liste de ruleID ou, si possible, String + next rule
type RuleMap = Map.Map Int (String, [Int])

-- pattern as a sequence of choices
-- If [Pattern] is [], end of Pattern
data Pattern = EndPattern | SeqP String [Pattern]
  deriving Show


-- ruleToPattern
genPatFromBody :: [(Int, RuleBody)] -> Pattern -> RuleBody -> Pattern
genPatFromBody rules EndPattern (Pat sPat) = SeqP sPat [EndPattern]
genPatFromBody rules (SeqP str patterns) (Pat sPat) = SeqP (sPat ++ str) patterns
genPatFromBody rules nextPat (IdR idr) = SeqP "" newPatterns
  where
    newPatterns = map (genPatFromSeq rules nextPat) idr

genPatFromSeq :: [(Int, RuleBody)] -> Pattern -> [Int] -> Pattern
genPatFromSeq rules nextPat [] = nextPat
genPatFromSeq rules nextPat (i:is) = case lookup i rules of
      Just body -> genPatFromBody rules (genPatFromSeq rules nextPat is) body
      Nothing -> error ("No rules num " ++ show i)

-- matchPattern
matchPattern :: String -> Pattern -> Bool
matchPattern "" EndPattern = True
matchPattern _ EndPattern = False
matchPattern str (SeqP pat patterns) = if startswith pat str
  then any (matchPattern (drop (length pat) str)) patterns
  else False
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

{--
8: 42 | 42 8
11: 42 31 | 42 11 31

but rules 8 and 11 are only used in
0: 8 11
--}

allMatch :: Pattern -> [[Char]]
allMatch (SeqP str patterns) = [str ++ pat | pat <- nextPat]
  where nextPat = concat $ map allMatch patterns
allMatch EndPattern = [""]

-- length of str is k1 * l42 + k2 (l42 + l31), k2 > 1 et k2 < (length str / (l31 + l42)) / 2
recurMatch :: Int -> Pattern -> Pattern -> [Char] -> Bool
recurMatch lpat pat42 pat31 str = any (\k -> matchStart k && matchEnd k) [1 .. maxK2]
  where
    maxK2 = div (length str - lpat) (2 * lpat)
    nbBlock = div (length str) lpat
    matchStart k = matchN lpat pat42 (nbBlock - k) (take (lpat * (nbBlock - k)) str)
    matchEnd k = matchN lpat pat31 k (drop (lpat * (nbBlock - k)) str)

matchN :: (Eq t, Num t) => Int -> Pattern -> t -> [Char] -> Bool
matchN lpat pat 0 "" = True
matchN lpat pat n str = matchPattern start pat && matchN lpat pat (n-1) end
  where
    (start, end) = splitAt lpat str
