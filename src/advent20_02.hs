{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC
-- startswith, join, replace, split
import Data.String.Utils ( split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- fromJust, fromMaybe, catMaybes, isNothing, listToMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( find, sortOn, groupBy ) -- sort, group, find, delete, sortOn, groupBy ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
-- import qualified Data.Bits as Bits

import Text.Read (readMaybe)

-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace

exInput :: String
exInput = "1-3 a: abcde"

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 02 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input02.txt"
  -- content <- readFile "Input20/test02_1.txt"

  let entries = map parseLine (lines content)
  -- apply validPasswd on a pair
  let valid = filter (uncurry validPasswd) entries
  -- print $ "valid=" ++ show valid

  let pRes = length valid
  putStrLn $ "Answer 1> " ++ show pRes

  let newValid = filter (uncurry newValidPasswd) entries
  let cRes = length newValid
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Constraint = (Int, Int, Char)

parseLine :: String -> (Constraint, String)
parseLine line = ((minNb, maxNb, letter), passwd)
  where
    tokCol = split ":" line
    passwd = tail (head ( tail tokCol ))  -- remove starting ' '
    tokSp = split " " (head tokCol)
    letter = head $ head (tail tokSp)
    tokMi = split "-" (head tokSp)
    minNb = read (head tokMi) :: Int
    maxNb = read (head (tail tokMi)) :: Int

countLetter :: Char -> String -> Int
countLetter letter passwd = length (filter (== letter) passwd)

validPasswd :: Constraint -> String -> Bool
validPasswd (cmin, cmax, letter) passwd = cmin <= nbLet && nbLet <= cmax
  where
    nbLet = countLetter letter passwd

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

newValidPasswd :: Constraint -> String -> Bool
newValidPasswd (p1, p2, letter) passwd = length (filter (==letter) extractedLetters) == 1
  where
    extractedLetters = [passwd !! (p1-1), passwd !! (p2-1)]

-- *****************************************************************************
-- *********************************************************************** Swmart
-- *****************************************************************************

-- Parsing using a do-block tricks with Maybe
data Policy = P
    { pIx1  :: Int
    , pIx2  :: Int
    , pChar :: Char
    , pPass :: String
    }
  deriving Show

parsePolicy :: String -> Maybe Policy
parsePolicy str = do
    -- 'words' split on ' ', 'c:_' takes only head
    [ixes,c:_,pwd] <- pure $ words str
    [ix1,ix2]      <- pure $ split "-" ixes
    P <$> readMaybe ix1
      <*> readMaybe ix2
      <*> pure c
      <*> pure pwd
-- if fact, even smarter
-- import           AOC.Common                 (countTrue, CharParser, parseLines)
-- policy :: CharParser Policy
-- olicy = P <$> decimal
--            <*> (char '-' *> decimal)
--            <*> (space *> anySingle)
--            <*> (char ':' *> space *> some anySingle)

-- and re-use (in validate 1 and 2) and for counting at the end
countTrue :: (a -> Bool) -> [a] -> Int
countTrue p = length . filter p
