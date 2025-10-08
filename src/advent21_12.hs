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
import Data.Char (isUpper)
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- ****** Control.Monad ( fold, guard )
import Control.Monad (guard)
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List.Extra (splitOn)
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)
import Data.List.Utils (strFromAL)

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
  putStrLn "** Advent 2021 - Day 12 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let graphMap = foldr parseEdge Map.empty (lines content)
  -- putStrLn $ "graphMap=" ++ show graphMap

  -- let paths = nTimesM (growPath graphMap) 10 [Start]
  -- putStrLn $ "paths=" ++ show paths

  let vPaths = untilM completePath (growPath validNodeToAdd graphMap) [Start]
  -- putStrLn $ "vPaths=" ++ show vPaths
  putStrLn $ "paths\n" ++ concatMap (\p -> nicePath p ++ "\n") vPaths
  let pRes = length vPaths
  putStrLn $ "Answer 1> " ++ show pRes

  let vPaths2 = untilM completePath (growPath isValidSmallPath graphMap) [Start]
  -- putStrLn $ "vPaths=" ++ show vPaths
  -- putStrLn $ "paths\n" ++ concatMap (\p -> nicePath p ++ "\n") vPaths2
  let cRes = length vPaths2
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Label = Start | End | Small String | Big String
  deriving (Eq, Ord)
instance Show Label
  where
    show Start     = "START"
    show End       = "END"
    show (Small l) = l
    show (Big l)   = l

type GraphMap = Map.Map Label [Label]

nicePath :: [Label] -> String
nicePath path = "P: " ++ concatMap (\l -> show l ++ ", ") (reverse path) ++ "--"


toLabel :: String -> Label
toLabel "start" = Start
toLabel "end"   = End
toLabel str = if isUpper (head str) then Big str else Small str

parseEdge :: String -> GraphMap -> GraphMap
parseEdge line gMap = addEdge l1 l2 (addEdge l2 l1 gMap)
  where
    (e1:e2:es) = splitOn "-" line
    l1 = toLabel e1
    l2 = toLabel e2
    addEdge _src _dst gm = if _dst /= Start then Map.insertWith (++) _src [_dst] gm
                                            else gm

-- cannot add a "Small node" in a path that already has one
validNodeToAdd :: [Label] -> Label -> Bool
validNodeToAdd path (Small lab) = notElem (Small lab) path
validNodeToAdd _ _ = True

growPath :: ([Label] -> Label -> Bool) -> Map.Map Label [Label] -> [Label] -> [[Label]]
growPath _ _ (End:es) = do return (End:es)
growPath cond gMap path = do
  let label = head path
  next <- gMap Map.! label
  guard (cond path next)
  return (next:path)

nTimesM :: (Ord t, Num t, Monad m) => (a -> m a) -> t -> a -> m a
nTimesM f n item
  | n > 0 = f item >>= nTimesM f (n-1)
  | otherwise = return item

completePath :: [Label] -> Bool
completePath (End:_) = True
completePath _ = False

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM cond f item = do
  let finished = cond item
  if finished then return item
              else f item >>= untilM cond f

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
countSmall :: [Label] -> Map.Map String Int
countSmall path = foldr opCount Map.empty path
  where
    opCount :: Label -> Map.Map String Int -> Map.Map String Int
    opCount (Small lab) cMap = Map.insertWith (+) lab 1 cMap
    opCount _ cMap = cMap

isValidSmallPath :: [Label] -> Label -> Bool
isValidSmallPath [] _ = True
isValidSmallPath path (Small lab) = Map.size cMap == 0 || maxElem < 2 || Map.findWithDefault 0 lab cMap < 1
  where
    cMap = countSmall path
    maxElem = maximum (Map.elems cMap)
isValidSmallPath _ _ = True
