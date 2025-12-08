{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant bracket" #-}
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
-- import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid     ( First(..) )
-- import qualified Linear.V2 as LV
import qualified Linear as L
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List.Extra ( delete, foldl', partition, sortBy, sortOn, splitOn )
import Data.Ord ( comparing, Down(..) )
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
  putStrLn "** Advent 2025 - Day 08 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let pts = map parseVec (lines content)
  let ds = sortOn fst $ makeDList pts
  -- putStrLn $ "ds=" ++ show ds

  let df = foldl' (\l d -> addLink l (snd d)) [] (take 10 ds)
  putStrLn $ "df=" ++ show df
  let sizes = sortBy (comparing Down) $ map Set.size df
  putStrLn $ "sizes=" ++ show sizes
  let pRes = product (take 3 sizes)
  putStrLn $ "Answer 1> " ++ show pRes

  let jNodes = junctionNodes ([], pts) ds
  putStrLn $ "jNodes=" ++ show jNodes
  let (L.V3 x1 _ _, L.V3 x2 _ _) = jNodes
  let cRes = x1 * x2
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Pt = L.V3 Float
parseVec line = L.V3 (read x) (read y) (read z)
  where
    x:y:z:_ = splitOn "," line

-- build ordered list of (dist, (n1, n2))
makeDList :: (L.Metric f, Floating a) => [f a] -> [(a, (f a, f a))]
makeDList [] = []
makeDList (p:ps) = map (\v -> (L.distance p v, (p,v))) ps ++ makeDList ps

-- A proper structure for graph/connections
type SetList = [Set.Set Pt]

addLink :: SetList -> (Pt, Pt) -> SetList
addLink ls (p1, p2) = Set.unions (Set.fromList [p1, p2] : withP1orP2) : without
  where
    (withP1orP2, without) = partition (\l -> Set.member p1 l || Set.member p2 l) ls

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
junctionNodes :: (SetList, [Pt]) -> [(Float, (Pt, Pt))] -> (Pt, Pt)
junctionNodes _ [] = error ("Empty distances")
junctionNodes (ls, pts) (d:ds)
  | length newLs == 1 && null newPts = (p1, p2)
  | otherwise = junctionNodes (newLs, newPts) ds
  where
    (_, (p1, p2)) = d
    newLs = addLink ls (p1, p2)
    newPts = delete p1 (delete p2 pts)
