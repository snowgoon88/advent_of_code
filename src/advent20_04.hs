{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC
import MyUtils (countTrue, groupLines)

-- import Data.String.Utils ( split, join, startswith) --, startswith, join, replace, split )
import Data.String.Utils (split)
import Text.Read (readMaybe)
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
import Data.Char (isDigit, isHexDigit, toLower)
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
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 04 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input04.txt"
  -- content <- readFile "Input20/test04_1.txt"
  -- content <- readFile "Input20/test04_2.txt"

  let groups = groupLines (lines content)
  -- print $ "groups=" ++ show groups

  let entries = map parseEntry groups
  -- putStrLn $ "entries" ++ show entries

  let valids = map validPass entries
  -- putStrLn $ "valids=" ++ show valids

  let pRes = countTrue id valids
  putStrLn $ "Answer 1> " ++ show pRes


  let validNew = map validPassport entries
  -- putStrLn $ "validNew=" ++ show validNew

  let cRes = countTrue id validNew
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
-- parse passport as list (key, val)
type Passport = [(String, String)]
neededKeys :: [String]
neededKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- entry is a list of empty separated key:val patterns
parseEntry :: [String] -> Passport
parseEntry entry = concat $ map parseEntryStr entry

parseEntryStr :: String -> Passport
parseEntryStr pat = map extract (words pat)
  where
    extract str = (head tok, head (tail tok))
      where tok = split ":" str

getKeys :: Passport -> [String]
getKeys pass = map fst pass

validPass :: Passport -> Bool
validPass pass = all id $ map (\k -> k `elem` keys) neededKeys
  where keys = getKeys pass

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- use the 'lookup' function to emulate Passport as a record

validPassport :: Passport -> Bool
validPassport pass = all id $ map (\k -> validKey pass k) neededKeys

validKey :: Passport -> String -> Bool
validKey pass "byr" = case lookup "byr" pass of
  Nothing -> False
  Just str -> forDigit 1920 2002 str
validKey pass "iyr" = case lookup "iyr" pass of
  Nothing -> False
  Just str -> forDigit 2010 2020 str
validKey pass "eyr" = case lookup "eyr" pass of
  Nothing -> False
  Just str -> forDigit 2020 2030 str
validKey pass "hgt" = case lookup "hgt" pass of
  Nothing -> False
  Just str -> validHeight str
validKey pass "hcl" = case lookup "hcl" pass of
  Nothing -> False
  Just str -> validHairColor str
validKey pass "ecl" = case lookup "ecl" pass of
  Nothing -> False
  Just str -> validEyeColor str
validKey pass "pid" = case lookup "pid" pass of
  Nothing -> False
  Just str -> validPID str
validKey _ _ = True

forDigit :: Int -> Int -> String -> Bool
forDigit minV maxV str = case readMaybe str :: Maybe Int of
  Nothing -> False
  Just val -> val >= minV && val <= maxV

validHeight :: String -> Bool
validHeight str = goodUnit (break (not . isDigit) str)
  where
    goodUnit :: (String, String) -> Bool
    goodUnit (nbS, unitS)
      | unitS == "cm" = case readMaybe nbS :: Maybe Int of
          Nothing -> False
          Just val -> val >= 150 && val <= 193
      | unitS == "in" = case readMaybe nbS :: Maybe Int of
          Nothing -> False
          Just val -> val >= 59 && val <= 76
      | otherwise = False

validHairColor :: String -> Bool
validHairColor str = case head str of
  '#' -> length str == 7 && (all id $ map isHexDigit (map toLower (tail str)))
  _   -> False

validEyeColor :: String -> Bool
validEyeColor str = str `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPID :: String -> Bool
validPID str = (length str == 9) && (all id $ map isDigit str)


-- *****************************************************************************
-- *********************************************************************** Smart
-- *****************************************************************************

-- make use of the Refined module () to parse type while checking constraints.
-- deep knowledge of Parse, Applicative, Monoid, etc...
