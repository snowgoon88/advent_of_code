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
import qualified MyUtils as MU

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
-- import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List.Extra ( splitOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)
import Linear (_m22)

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
  putStrLn "** Advent 2025 - Day 05 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let groupsOfInput = MU.groupLines (lines content)
  let allR = map parseRange (head groupsOfInput)
  -- putStrLn $ "allR=" ++ show allR

  let allID = map read (groupsOfInput !! 1) :: [Int]
  -- putStrLn $ "allID=" ++ show allID
  let fresh = map (isFresh allR) allID
  -- putStrLn $ "fresh=" ++ show fresh
  let pRes = MU.countTrue id fresh
  putStrLn $ "Answer 1> " ++ show pRes

  let maxRng = maxFresh allR
  putStrLn $ "nb ranges=" ++ show (length allR)
  putStrLn $ "maxRng=" ++ show maxRng

  let setAllR = Set.fromList allR
  -- putStrLn $ "setAllR=" ++ show setAllR
  -- let set00 = Set.empty :: Set.Set Range
  -- putStrLn $ "set00=" ++ show set00

  -- let set01 = unionRange set00 (Set.elemAt 0 setAllR)
  -- putStrLn $ "set01" ++ show set01
  -- -- let set02 = unionRange (fst set01) (Set.elemAt 1 setAllR)
  -- -- putStrLn $ "set02" ++ show set02
  -- -- let set03 = unionRange (fst set02) (Set.elemAt 2 setAllR)
  -- -- putStrLn $ "set03" ++ show set03
  -- -- let set04 = unionRange (fst set03) (Set.elemAt 3 setAllR)
  -- -- putStrLn $ "set04" ++ show set04
  -- let set02 = unionRange set01 (Set.elemAt 1 setAllR)
  -- putStrLn $ "set02" ++ show set02
  -- let set03 = unionRange set02 (Set.elemAt 2 setAllR)
  -- putStrLn $ "set03" ++ show set03
  -- let set04 = unionRange set03 (Set.elemAt 3 setAllR)
  -- putStrLn $ "set04" ++ show set04

  let finalSet = foldr unionRange Set.empty setAllR
  let cRes = sum $ map nbInRange (Set.toList finalSet)

  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Range = R { rMin :: Int
               , rMax :: Int }
  deriving ( Show )
instance Eq Range where
  (R min1 max1) == (R min2 max2) = min1 == min2 && max1 == max2
instance Ord Range where
  (R m1 _) <= (R m2 _) = m1 <= m2

parseRange :: String -> Range
parseRange str = R (read low) (read high)
  where
    (low:high:_) = splitOn "-" str

inRange :: Range -> Int -> Bool
inRange rng val = val >= (rMin rng) && val <= (rMax rng)

isFresh :: [Range] -> Int -> Bool
isFresh rngList idVal = any (\r -> inRange r idVal) rngList

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
maxFresh rngList = maximum (map rMax rngList)

-- setRng is an ordered Partition (empty intersection) of Ranges
-- unionRange :: Set.Set Range -> Range -> Set.Set Range
-- unionRange setRng rng = (Set.unions [sInf, Set.singleton combinedRange, sAfter], (sInf, sDropped, sToConsider, sAfter))
unionRange :: Range -> Set.Set Range -> Set.Set Range
unionRange rng setRng = Set.unions [sInf, Set.singleton combinedRange, sAfter] --, (sInf, sDropped, sToConsider, sAfter))
  where
    (sInf, sOpen) = Set.partition (\r -> rMax r < rMin rng) setRng
    (sDropped, sOpenHigh) = Set.partition (isIncluded rng) sOpen
    (sToConsider, sAfter) = Set.partition (\r -> (rMin r) <= (rMax rng)) sOpenHigh
    combinedRange = foldr combineWith rng (Set.toList sToConsider)

isIncluded :: Range -> Range -> Bool
isIncluded rBig rSmall = (rMin rSmall) >= (rMin rBig) && (rMax rSmall) <= (rMax rBig)

-- combine with non null intersection
combineWith :: Range -> Range -> Range
combineWith r1 r2 = R (min (rMin r1) (rMin r2)) (max (rMax r1) (rMax r2))

nbInRange :: Range -> Int
nbInRange (R low high) = high - low + 1
