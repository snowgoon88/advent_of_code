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

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.Foldable ( foldl' )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List ( foldl', elemIndex, elemIndices )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)

-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug :: c -> String -> c
-- debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2025 - Day 07 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let tachLines = lines content
  putStrLn $ "tachLines=" ++ show tachLines

  let b00 = initBeam (head tachLines)
  -- putStrLn $ "b00=" ++ show b00
  -- let (b01, n01) = stepDown (b00, 0) (tachLines !! 1)
  -- putStrLn $ "s01=" ++ show n01 ++ ": " ++ show b01
  -- let (b02, n02) = stepDown (b01, n01) (tachLines !! 2)
  -- putStrLn $ "s02=" ++ show n02 ++ ": " ++ show b02
  let res = foldl' stepDown (b00, 0) (drop 1 tachLines)
  putStrLn $ "res=" ++ show res
  let pRes = snd res
  putStrLn $ "Answer 1> " ++ show pRes

  -- brute force !!
  -- let t00 = initTimelines (head tachLines)
  -- let tf = foldl' nextTimelines t00 (drop 1 tachLines)
  -- putStrLn $ "tf=" ++ show tf
  -- let cRes = length tf

  let tln00 = initTLN (head tachLines)
  -- putStrLn $ "tln00=" ++ show tln00
  -- let tln01 = nextTLNb tln00 (tachLines !! 1)
  -- putStrLn $ "tln01=" ++ show tln01
  -- let tln02 = nextTLNb tln01 (tachLines !! 2)
  -- putStrLn $ "tln02=" ++ show tln02
  let tf = foldl' nextTLNb tln00 (drop 1 tachLines)
  putStrLn $ "tf=" ++ show tf
  let cRes = nbTimelines tf
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Beam = Map.Map Int Int

initBeam :: String -> Beam
initBeam line = Map.fromList $ zip [0..length line] (map (\c -> if c=='S' then 1 else 0) line)

stepDown :: (Beam, Int) -> String -> (Beam, Int)
stepDown (beam, nbSplit) tLine = foldr opSplit (beam, nbSplit) idxSplit
  where
    idxSplit = elemIndices '^' tLine
    opSplit idx (b, acc)
      | b Map.! idx > 0 = (Map.adjust (+1) (idx-1)
                           (Map.adjust (const 0) idx
                            (Map.adjust (+1) (idx+1) b)), acc+1)
      | otherwise = (b, acc)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- brute force is a bit too long...
type TimeLines = [Int]

initTimelines :: String -> TimeLines
initTimelines line = elemIndices 'S' line

nextTimelines :: TimeLines -> String -> TimeLines
nextTimelines tl splitters = recSplit tl
  where
    idxSplit = elemIndices '^' splitters
    -- recursively compute next timesLInes position
    recSplit [] = []
    recSplit (t:ts)
      | t `elem` idxSplit = (t-1):(t+1): recSplit ts
      | otherwise = t : recSplit ts

-- at each position (Int), how many timelines ?
type TLNb = Map.Map Int Int

initTLN :: String -> TLNb
initTLN line = case elemIndex 'S' line of
  Just p -> Map.singleton p 1
  Nothing -> Map.empty

nextTLNb :: TLNb -> String -> TLNb
nextTLNb tlnb splitters = foldr opNextTL tlnb idxSplit
  where
    idxSplit = elemIndices '^' splitters

    opNextTL idx t = case Map.lookup idx tlnb of
      -- add to left(-1) and right(+1)
      Just nbTL -> Map.delete idx (Map.insertWith (+) (idx-1) nbTL (Map.insertWith (+) (idx+1) nbTL t))
      Nothing -> t

nbTimelines :: TLNb -> Int
nbTimelines tlnb = sum (Map.elems tlnb)
