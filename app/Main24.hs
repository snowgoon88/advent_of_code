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
-- import Data.String.Utils ( split, join) --, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
-- import Control.Monad ( fold )
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( delete, sortOn ) -- delete, sortOn
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
  putStrLn "** Advent 2024 - Day 18 Part - & -                                          **"
  putStrLn "********************************************************************************"
  -- content <- readFile "Input24/input18.txt"
  -- content <- readFile "Input24/test18_1.txt"

  putStrLn $ "Answer 1> " ++ show pRes

  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

