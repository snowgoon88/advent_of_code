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
import qualified MyGrid as MG
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines
import qualified MyUtils as MU

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
import Data.Char (digitToInt)
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
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
  putStrLn "** Advent 2021 - Day 11 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let (size, nrjMap) = readEnergyMap (lines content)
  -- let nrjPlus = levelUp (levelUp nrjMap)
  -- putStrLn $ "nrjPlus=" ++ show nrjPlus
  -- let (pFlash, newMap) = propagateFlashes (spontaneousFlash nrjPlus) ([], nrjPlus)
  -- putStrLn $ "pFlash=" ++ show pFlash
  -- let (nbFlashes, nrjMap01) = stepFlash ([], nrjMap)
  -- putStrLn $ "newMap=\n" ++ nice2DMap (snd size) nrjMap01
  -- putStrLn $ "nbFlashes=" ++ show nbFlashes
  -- let (nbFlashes02, nrjMap02) = stepFlash (nbFlashes, nrjMap01)
  -- putStrLn $ "newMap=\n" ++ nice2DMap (snd size) nrjMap02
  -- putStrLn $ "nbFlashes=" ++ show nbFlashes02
  let (nbFlashes, finalMap) = MU.applyN stepFlash 100 ([], nrjMap)
  putStrLn $ "newMap=\n" ++ nice2DMap (snd size) finalMap
  putStrLn $ "nbFlashes=" ++ show nbFlashes
  let pRes = sum nbFlashes
  putStrLn $ "Answer 1> " ++ show pRes

  let res = until allFlash stepFlash ([], nrjMap )
  let cRes = length $ fst res
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type EnergyMap = MG.PosMapCore Int
type PosList = [MG.Pos]

readEnergyMap :: [String] -> (MG.Size, EnergyMap)
readEnergyMap ls = (size, Map.map digitToInt charMap)
  where
    (size, charMap) = MG.readGrid ls

-- one step
stepFlash :: ([Int], EnergyMap) -> ([Int], EnergyMap)
stepFlash (nbFlashList, eMap) = (length posFlash : nbFlashList, finalMap)
  where
    leveled = levelUp eMap
    (posFlash, tmpMap) = propagateFlashes (spontaneousFlash leveled) ([], leveled)
    finalMap = resetFlashed posFlash tmpMap

neighbor8 :: [MG.DirVec]
neighbor8 = [(1, 0), (1, 1), (0, 1), (-1, 1),
             (-1, 0), (-1, -1), (0, -1), (1, -1)]

levelUp :: EnergyMap -> EnergyMap
levelUp eMap = Map.map (+1) eMap

spontaneousFlash :: EnergyMap -> PosList
spontaneousFlash eMap = Map.keys (Map.filter (== 10) eMap)

opPropagateFlash :: MG.Pos -> (PosList, EnergyMap) -> (PosList, EnergyMap)
opPropagateFlash pos (pList, eMap) = if newVal == 10 then (pos:pList, newMap)
                                                     else (pList, newMap)
  where
    newMap = Map.adjust (+1) pos eMap
    newVal = Map.findWithDefault 0 pos newMap

propagateFlashes :: PosList -> (PosList, EnergyMap) -> (PosList, EnergyMap)
propagateFlashes [] (seen, eMap) = (seen, eMap)
propagateFlashes (p:ps) (posSeen, eMap) = propagateFlashes (ps ++ newPosList) (p:posSeen, newMap)
  where
    (newPosList, newMap) = foldr (opPropagateFlash . MG.addDir p) ([], eMap) neighbor8

resetFlashed :: PosList -> EnergyMap -> EnergyMap
resetFlashed posSeen eMap = foldr (Map.adjust (const 0)) eMap posSeen

nice2DMap :: Show b => Int -> Map.Map a b -> String
nice2DMap hSize iMap = MG.chunks hSize strMap
  where
    strMap = concatMap (show . snd) (Map.toList iMap)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- stop Condition
allFlash :: ([Int], EnergyMap) -> Bool
allFlash ([], _) = False
allFlash (nbList, eMap) = head nbList == Map.size eMap
