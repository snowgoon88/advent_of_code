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
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
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

start = [9, 6, 0, 10, 18, 2, 1] :: [Int]

start1 = [0, 3, 6] :: [Int]
g1 = initGame start1

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 15 Part - & -                                          **"
  putStrLn "********************************************************************************"
  -- content <- readFile "Input20/input15.txt"
  -- content <- readFile "Input20/test15_1.txt"

  let game = initGame start
  putStrLn $ "game=" ++ show game

  let pRes = gVal $ until (\g -> (gT g) == 2021) stepGame game
  putStrLn $ "Answer 1> " ++ show pRes

  let cRes = gVal $ until (\g -> (gT g) == 30000001) stepGame game
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
-- Value -> previous times played, reversed order.
type LastTimes = Map.Map Int [Int]
data Game = G { gMem :: LastTimes
              , gT :: Int         -- time to play
              , gVal :: Int }     -- last val
  deriving (Show)
emptyGame :: Game
emptyGame = G (Map.empty) 1 0

-- | speak : update LastTime
speak :: Game -> Int -> Game
speak (G mem t _) val = G newMem (t+1) val
  where
    newMem = Map.insertWith (++) val [t] mem

-- | predict : the number to speak at time, given last val
predict :: Game -> Int
predict (G mem t val) = case Map.lookup val mem of
  Nothing -> 0 -- error in fact
  Just [t] -> 0
  Just (t:oldT:ts) -> t - oldT

initGame :: [Int] -> Game
initGame ns = foldl speak emptyGame ns

stepGame :: Game -> Game
stepGame g = speak g (predict g)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

