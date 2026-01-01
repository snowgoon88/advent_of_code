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
-- import qualified MySolver as MS
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.Foldable ( foldl', foldlM )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Data.Vector as V
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
--                   intercalate, transpose, partition
-- ****** Data.List.Extra: splitOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace, traceM )
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
  putStrLn "** Advent 2021 - Day xx Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  -- putStrLn $ "Answer 1> " ++ show pRes

  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

