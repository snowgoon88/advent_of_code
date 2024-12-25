{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC

-- import Data.String.Utils ( split, join, startswith) --, startswith, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( find, sortOn, groupBy ) -- sort, group, find, delete, sortOn, groupBy ) -- delete, sortOn
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
  putStrLn "** Advent 2024 - Day 25 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input25.txt"
  -- content <- readFile "Input24/test25_1.txt"

  -- regroup lines separated by [""]
  let contLines = filter (/= [""]) $ groupBy (\x y -> x /= "" && y /= "") (lines content)
  let patterns = map readGrid contLines

  -- print $ "size=" ++ show (fst (head patterns))
  -- mapM_ (putStrLn . mapToStr) patterns

  let (locks, keys) = foldl parsePatterns ([], []) patterns
  -- print $ "locks=" ++ show locks
  -- print $ "keys=" ++ show keys

  let pRes = findUniquePairs locks keys
  putStrLn $ "Answer 1> " ++ show pRes

  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type PatMap = GridMapCore Char
type Lock = [Int]
type Key = [Int]

mapToStr :: PatMap -> String
mapToStr (size, grid) = chunks (snd size) $ map snd (Map.toList grid)

parsePatterns :: ([Lock], [Key]) -> PatMap -> ([Lock], [Key])
parsePatterns (locks, keys) patMap
  | (snd patMap) Map.! (0, 0) == '#' = (newLock:locks, keys)
  | otherwise = (locks, newKey:keys)
  where
    newLock = parseLock patMap
    newKey = parseKey patMap

parseLock :: PatMap -> Lock
parseLock ((r,c), pat) = map (parseColDown ((r,c),pat) 0) [0..c-1]

parseColDown :: PatMap -> Int -> Int -> Int
parseColDown (size, pat) row col
  | pat Map.! (row+1,col) == '.' = row
  | otherwise = parseColDown (size, pat) (row+1) col

parseKey :: PatMap -> Key
parseKey ((r,c), pat) = map (parseColUp ((r,c),pat) 0) [0..c-1]

parseColUp :: PatMap -> Int -> Int -> Int
parseColUp ((r,c), pat) row col
  | pat Map.! (r-2-row, col) == '.' = row
  | otherwise = parseColUp ((r,c), pat) (row+1) col

fitLockKey :: Lock -> Key -> Bool
fitLockKey [] [] = True
fitLockKey (l:ls) (k:ks)
  | (l+k) > 5 = False
  | otherwise = fitLockKey ls ks

findUniquePairs :: [Lock] -> [Key] -> Int
findUniquePairs locks keys = length fitted
  where fitted = [(l,k) | l <- locks, k <-keys, fitLockKey l k]

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

