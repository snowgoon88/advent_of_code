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
import MyUtils as MU
-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.Foldable ( foldl' )
import Data.Foldable (foldl')
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
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
  putStrLn "** Advent 2025 - Day 01 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let rots = map parseRot (lines content)
  -- putStrLn $ "rots=" ++ show rots
  let seqs = makeSequence rots 50
  -- putStrLn $ "seq=" ++ show seqs

  let pRes = MU.countTrue (==0) seqs
  putStrLn $ "Answer 1> " ++ show pRes

  let seqsa = makeSequenceAdv rots 50
  -- putStrLn $ "seq=" ++ show seqsa
  let cRes = fst (head seqsa)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

parseRot :: String -> Int
parseRot ('R':ls) = read ls
parseRot ('L':ls) = - read ls
parseRot str = error ("Error parsing " ++ str)

opRot :: [Int] -> Int -> [Int]
opRot [] _ = error "opRot applied to []"
opRot (current:cs) rot = mod (current+rot) 100 : current : cs

makeSequence :: [Int] -> Int -> [Int]
makeSequence allRot start = foldl' opRot [start] allRot
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- nb of clicks in front of 0, result of dial
opClickDial :: [(Int, Int)] -> Int -> [(Int, Int)]
opClickDial [] _ = error "opClickDial applied to []"
opClickDial ((nbClick, dial):cs) rot = (newNbClick, newDial) : (nbClick, dial) : cs
  where
    (mul, res) = divMod (dial+rot) 100
    newNbClick
      | mul == 0 = nbClick + if res==0 then 1 else 0
      | mul > 0 = nbClick + mul
      | mul < 0 && dial == 0 = nbClick - 1 + (-mul) + if res==0 then 1 else 0
      | mul < 0 = nbClick - mul + if res==0 then 1 else 0
      | otherwise = nbClick
    newDial = res

makeSequenceAdv :: [Int] -> Int -> [(Int, Int)]
makeSequenceAdv allRot start = foldl' opClickDial [(0, start)] allRot
