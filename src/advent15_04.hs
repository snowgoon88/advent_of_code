{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
-- import Data.List ( sort ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust, catMaybes )
-- import Control.Monad ( fold )
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace

import qualified Data.Hash.MD5 as MD5


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2015 - Day 04 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  -- content <- readFile "Input15/input04.txt"
  -- content <- readFile "Input15/test04_1.txt"

  -- let pRes = fst $ firstHash "abcdef"
  let pRes = fst $ firstHash "iwrupvqb" lead5

  putStrLn $ "Answer 1> " ++ show pRes

  let cRes = fst $ firstHash "iwrupvqb" lead6
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

lead5 :: String -> Bool
lead5 ('0':'0':'0':'0':'0':str) = True
lead5 _ = False

toMD5 :: String -> String
toMD5 str = MD5.md5s $ MD5.Str str

-- firstHash :: String -> (String, String)
firstHash :: (Show a, Num a, Enum a) => String -> (String -> Bool) -> (a, String)
firstHash key func = head $ filter (func . snd) (map (\n -> (n, toMD5 (key ++ show n))) [1..])

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************


lead6 :: String -> Bool
lead6 ('0':'0':'0':'0':'0':'0':str) = True
lead6 _ = False
