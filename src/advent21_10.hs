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
import Data.Maybe (mapMaybe, isNothing)
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List (sort)
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
  putStrLn "** Advent 2021 - Day 10 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let corrupted = mapMaybe (fst . checkCorrupted []) (lines content)
  putStrLn $ "corrupted=" ++ show corrupted

  let pRes = sum $ map costOpen corrupted
  putStrLn $ "Answer 1> " ++ show pRes

  let incomplete = map snd (filter (isNothing . fst) $ map (checkCorrupted []) (lines content))
  -- putStrLn $ "incomplete=" ++ show incomplete
  let scoreClose = map (scoreComplete 0) incomplete
  putStrLn $ "scoreClose" ++ show scoreClose
  let sortedScore = sort scoreClose

  let cRes = sortedScore !! div (length sortedScore) 2
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Chunk = OpenP | OpenQ | OpenC | OpenB
  deriving (Show)

type CheckStack = [Chunk]

costOpen :: Char -> Int
costOpen ')' = 3
costOpen ']' = 57
costOpen '}' = 1197
costOpen '>' = 25137
costOpen _ = 0

checkCorrupted :: CheckStack -> String -> (Maybe Char, CheckStack)
checkCorrupted stack [] = (Nothing, stack)
-- open
checkCorrupted stack ('(':ps) = checkCorrupted (OpenP:stack) ps
checkCorrupted stack ('[':ps) = checkCorrupted (OpenQ:stack) ps
checkCorrupted stack ('<':ps) = checkCorrupted (OpenC:stack) ps
checkCorrupted stack ('{':ps) = checkCorrupted (OpenB:stack) ps
-- close
checkCorrupted (OpenP:stack) (')':ps) = checkCorrupted stack ps
checkCorrupted (OpenQ:stack) (']':ps) = checkCorrupted stack ps
checkCorrupted (OpenC:stack) ('>':ps) = checkCorrupted stack ps
checkCorrupted (OpenB:stack) ('}':ps) = checkCorrupted stack ps
-- corrupted
checkCorrupted stack (p:_) = (Just p, stack)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

scoreComplete :: Int -> CheckStack -> Int
scoreComplete score [] = score
scoreComplete score (OpenP:stack) = scoreComplete (1+5*score) stack
scoreComplete score (OpenQ:stack) = scoreComplete (2+5*score) stack
scoreComplete score (OpenB:stack) = scoreComplete (3+5*score) stack
scoreComplete score (OpenC:stack) = scoreComplete (4+5*score) stack
