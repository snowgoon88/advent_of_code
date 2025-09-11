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
import Data.String.Utils (startswith)
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
--import qualified Linear.V2 as L2
--import qualified Linear.V3 as L3
import qualified Linear as L
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
  putStrLn "** Advent 2021 - Day 02 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input21/input02.txt"
  -- content <- readFile "Input21/test02_1.txt"

  let posFinal = foldl applyCmd (L.V2 0 0) (map parseCmd (lines content))
  putStrLn $ "posFinal=" ++ show posFinal
  let (L.V2 f d) = posFinal
  let pRes = f * d
  putStrLn $ "Answer 1> " ++ show pRes

  let posFinal3 = foldl applyCmdAimed (L.V3 0 0 0) (map parseCmd (lines content))
  putStrLn $ "posFinal3=" ++ show posFinal3
  let (L.V3 f d _) = posFinal3
  let cRes = f * d
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Cmd a = For a | Do a | Up a
  deriving (Show)
type SubCmd = Cmd Int

parseCmd :: String -> SubCmd
parseCmd line
  | startswith "forward" line = For nb
  | startswith "down" line = Do nb
  | startswith "up" line = Up nb
  | otherwise = error ("Cannot parse: " ++ line )
  where
    nb = read (words line !! 1)

type Pos = L.V2 Int
applyCmd :: Pos -> SubCmd -> Pos
applyCmd pos (For x) = (x L.*^ L.V2 1 0) L.^+^ pos
applyCmd pos (Do x) = (x L.*^ L.V2 0 1) L.^+^ pos
applyCmd pos (Up x) = (x L.*^ L.V2 0 (-1)) L.^+^ pos


-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
type AimedPos = L.V3 Int

applyCmdAimed :: AimedPos -> SubCmd -> AimedPos
applyCmdAimed (L.V3 f d a) (For x) = L.V3 (f+x) (d+a*x) a
applyCmdAimed (L.V3 f d a) (Do x) = L.V3 f d (a+x)
applyCmdAimed (L.V3 f d a) (Up x) = L.V3 f d (a-x)
