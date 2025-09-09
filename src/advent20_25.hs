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
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
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
  putStrLn "** Advent 2020 - Day 25 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input25.txt"
  -- content <- readFile "Input20/test25_1.txt"

  let kCard = read (head (lines content)) :: Integer
  let kDoor = read (lines content !! 1) :: Integer

  let keys = makeLoop 7 11
  putStrLn $ "keys=" ++ show keys
  let kboth = findLoops (kCard, kDoor) (Nothing, Nothing) 1 1
  putStrLn $ "kboth=" ++ show kboth

  let pRes = head (makeLoop kCard (snd kboth))
  putStrLn $ "Answer 1> " ++ show pRes

  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

makeLoop :: (Integral a1, Num a2, Enum a2) => a1 -> a2 -> [a1]
makeLoop sn nb = foldr (\_ acc -> cryptLoop sn (head acc) : acc) [1] [0 .. nb-1]

findLoops (_, _) (Just v1, Just v2) _ _ = (v1, v2)
findLoops (k1, k2) (Just v1, Nothing) key nb = findLoops (k1, k2) (Just v1, nv2) newKey (nb+1)
  where
    newKey = cryptLoop 7 key
    nv2 = if k2 == newKey then Just nb else Nothing
findLoops (k1, k2) (Nothing, Just v2) key nb = findLoops (k1, k2) (nv1, Just v2) newKey (nb+1)
  where
    newKey = cryptLoop 7 key
    nv1 = if k1 == newKey then Just nb else Nothing
findLoops (k1, k2) (Nothing, Nothing) key nb = findLoops (k1, k2) (nv1, nv2) newKey (nb+1)
  where
    newKey = cryptLoop 7 key
    nv1 = if k1 == newKey then Just nb else Nothing
    nv2 = if k2 == newKey then Just nb else Nothing

cryptLoop :: Integral a => a -> a -> a
cryptLoop sn n = mod (n * sn) 20201227


-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

