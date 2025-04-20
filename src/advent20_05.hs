{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- ****** Data.List: sort, group, find, delete, (\\), sortOn, groupBy, foldl'
import Data.List ((\\), foldl')
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
  putStrLn "** Advent 2020 - Day 05 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input05.txt"
  -- content <- readFile "Input20/test05_1.txt"

  let id1 = map seatID (lines content)
  -- putStrLn $ "id1=" ++ show id1
  let pRes = maximum id1
  putStrLn $ "Answer 1> " ++ show pRes

  let leftSeats = [1..pRes] \\ id1
  putStrLn $ "leftSeats=" ++ show leftSeats

  let cRes = findSeat (lines content)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

seatID :: String -> Int
seatID seatStr = row * 8 + col
  where (row, col) = seatDecode seatStr

seatDecode :: String -> (Int, Int)
seatDecode seatStr = (rowDecode (take 7 seatStr), colDecode (drop 7 seatStr))

-- decodeRow, 7 bit with F=0, B=1
rowCode :: [Int]
rowCode = [64, 32, 16, 8, 4, 2, 1]
rowDecode :: String -> Int
rowDecode rowStr = sum $ map (\(s,c) -> if s == 'B' then c else 0) (zip rowStr rowCode)

-- decodeCol, 7 bit with L=0, R=1
colCode :: [Int]
colCode = [4, 2, 1]
colDecode :: String -> Int
colDecode colStr = sum $ map (\(s,c) -> if s == 'R' then c else 0) (zip colStr colCode)

-- SMART
foldDecode :: String -> Int
foldDecode str = foldl' opFold 0 str
  where
    opFold acc letter = case letter of
      'B' -> 2 * acc + 1
      'F' -> 2 * acc
      'R' -> 2 * acc + 1
      'L' -> 2 * acc
      _   -> 2 * acc


-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- can get this seat by 1) theoretical sum between min and max, 2) remove actual sum
-- NOT efficient
findSeat :: [String] -> Int
findSeat seatStrs = missing
  where
    ids = map seatID seatStrs
    minS = minimum ids
    maxS = maximum ids
    missing = (maxS)*(maxS+1) `div` 2 - (minS-1)*(minS) `div` 2 - (sum ids)

{- Ce qu'on peut aussi écrire comme ça avec Control.Foldl
{-# LANGUAGE ApplicativeDo #-}
import qualified Control.Foldl as F

findHole :: F.Fold Int (Maybe Int)
findHole = do
    mn <- F.minimum
    mx <- F.maximum
    sm <- F.sum
    pure $
      missingItem <$> mn <*> mx <*> pure sm
  where
    missingItem mn mx sm = totalSum - sm
      where
        totalSum = mx*(mx+1)`div`2 - mn*(mn-1)`div`2
-}
