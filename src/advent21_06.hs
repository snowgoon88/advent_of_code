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
-- ****** MyUtils: applyN, countTrue, groupLines
import MyUtils as MU

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
import Data.String.Utils (split)
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
import qualified Linear as L
import qualified Data.Vector.Mutable as VUM
import qualified Data.Vector as V
import Control.Monad.ST (runST)
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
-- ****** Data.List.Extra: splitOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)

-- *********************************************************************************** DEBUG
-- trace :: String > a -> a
import Debug.Trace ( trace )
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug :: c -> String -> c
debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2021 - Day 06 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"

  let vInit = readVector content
  putStrLn $ "vInit=" ++ show vInit

  let endFish = MU.applyN step 80 vInit
  putStrLn $ "endFIsh" ++ show endFish
  let pRes = sum endFish
  putStrLn $ "Answer 1> " ++ show pRes

  let endFishBig = MU.applyN step 256 vInit
  putStrLn $ "endFishBig" ++ show endFishBig
  let cRes = sum endFishBig
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Fishes = V.Vector Int
type TMat = V.Vector (V.Vector Int)

-- read the list of fish's timer into a mutable Vector (as read from IO)
-- using the ST monad.
readVector :: String -> Fishes
readVector line = runST $ do
  -- initial vector, everything to 0
  v <- VUM.replicate 9 (0 :: Int)  -- age from 0 to 8
  mapM_ (\n_str -> do
            let n = read n_str
            val <- VUM.read v n
            VUM.write v n (val+1)
        ) $ split "," line
  V.freeze v

-- the Transition 2D Matrix, as Vector (Vector Int)
-- Matrix !* (Column Vector)
-- V2 (V3 1 2 3) (V3 4 5 6) !* V3 7 8 9
transMat :: TMat
transMat = V.fromList [ V.fromList [0, 1, 0, 0, 0, 0, 0, 0, 0]
                      , V.fromList [0, 0, 1, 0, 0, 0, 0, 0, 0]
                      , V.fromList [0, 0, 0, 1, 0, 0, 0, 0, 0]
                      , V.fromList [0, 0, 0, 0, 1, 0, 0, 0, 0]
                      , V.fromList [0, 0, 0, 0, 0, 1, 0, 0, 0]
                      , V.fromList [0, 0, 0, 0, 0, 0, 1, 0, 0]
                      , V.fromList [1, 0, 0, 0, 0, 0, 0, 1, 0]
                      , V.fromList [0, 0, 0, 0, 0, 0, 0, 0, 1]
                      , V.fromList [1, 0, 0, 0, 0, 0, 0, 0, 0]
                      ]

step :: Fishes -> Fishes
step f = transMat L.!* f


-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

