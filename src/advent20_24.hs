{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main20 where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines, applyN
import qualified MyUtils as MU

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
-- import qualified Data.Set as Set
import qualified Linear.V2 as L
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
  putStrLn "** Advent 2020 - Day 24 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input24.txt"
  -- content <- readFile "Input20/test24_1.txt"

  let flippedHexes = fillHexMap (lines content)
  putStrLn $ "flippedHex=" ++ show flippedHexes

  let listBlack = Map.filter odd flippedHexes
  let pRes = Map.size listBlack
  putStrLn $ "Answer 1> " ++ show pRes

  -- putStrLn $ "blacks00=" ++ niceBlacks (Map.keys listBlack)
  -- -- let step01 = computeMarks (Map.keys flippedHexes)
  -- -- putStrLn $ "step01=" ++ niceHexMap step01
  -- let blacks01 = newBlacks (Map.keys listBlack)
  -- putStrLn $ "blacks01=(" ++ niceBlacks blacks01
  -- let blacks02 = newBlacks blacks01
  -- putStrLn $ "blacks02=(" ++ niceBlacks blacks02
  -- let blacks02 = newBlacks blacks01
  -- putStrLn $ "blacks02=(" ++ niceBlacks blacks02

  let blackFinal = MU.applyN newBlacks 100 (Map.keys listBlack)
  let cRes = length blackFinal
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type HPos = L.V2 Int
type HexMap = Map.Map HPos Int

codeToMove :: String -> (HPos, String)
codeToMove [] = (L.V2 0 0, [])
codeToMove ('e':ps) = (L.V2 1 0, ps)
codeToMove ('n':'e':ps) = (L.V2 0 1, ps)
codeToMove ('n':'w':ps) = (L.V2 (-1) 1, ps)
codeToMove ('w':ps) = (L.V2 (-1) 0, ps)
codeToMove ('s':'w':ps) = (L.V2 0 (-1), ps)
codeToMove ('s':'e':ps) = (L.V2 1 (-1), ps)

endPoint :: HPos -> String -> HPos
endPoint pos [] = pos
endPoint pos pathCode = endPoint (pos+move) endPath
  where
    (move, endPath) = codeToMove pathCode

fillHexMap :: [String] -> HexMap
fillHexMap paths = foldl flipHex Map.empty paths

flipHex :: HexMap -> String -> HexMap
flipHex hexMap path = Map.insertWith (+) finalHex 1 hexMap
  where
    finalHex = endPoint (L.V2 0 0) path
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- for each black Tile, (+1) to each neighbors.
-- then check the number for black: 0 or >2 -> flipped (i.e. not kept)
-- and add any new white with 2 marks.

-- keep tiles with 2 marks, and also blacks with 1
newBlacks posList = withTwo ++ blacksWithOne
  where
    markedTilesMap = computeMarks posList
    withTwo = Map.keys (Map.filter (==2) markedTilesMap)
    blacksWithOne = Map.keys $ Map.filterWithKey (\k n -> k `elem` posList && n == 1) markedTilesMap

-- add Marks around a list of blak Tiles
computeMarks :: [HPos] -> HexMap
computeMarks posList = foldl opMark Map.empty posList

opMark :: HexMap -> HPos -> HexMap
opMark hMap pos = foldl updateNeighbors hMap posNeighbors
  where
    updateNeighbors hm p = Map.insertWith (+) (p+pos) 1 hm
posNeighbors :: [HPos]
posNeighbors = [L.V2 1 0, L.V2 0 1, L.V2 (-1) 1, L.V2 (-1) 0, L.V2 0 (-1), L.V2 1 (-1)]

niceHex (L.V2 x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
niceBlacks posList = "[" ++ show (length posList) ++ "] " ++
  foldr (\p msg -> msg ++niceHex p ++ " // ") "" posList
niceHexMap hMap = foldr opNice "" (Map.toList hMap)
  where
    opNice (k, v) msg = msg ++ niceHex k ++ ": " ++ show v ++ " // "
