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
import Data.Maybe ( mapMaybe )
-- ****** Data.IntSet: fromList, toList, split
import Data.Foldable ( foldl' )
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
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
import Debug.Trace ( trace ) -- trace :: String > a -> a
import MyParser (Parser(val))
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
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
  putStrLn "** Advent 2021 - Day 16 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        ['_':ns] -> return ns
        [fileName] -> readFile ("Input21/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  -- let binStr = hexToBinary (head (lines content))
  -- putStrLn $ "bin=" ++ show binStr
  -- let (version, bin00) = parseAsBinary 3 binStr
  -- putStrLn $ "version=" ++ show version
  -- let (typeID, bin01) = parseAsBinary 3 bin00
  -- putStrLn $ "typeID=" ++ show typeID
  -- let (litt, _, bin02) = parseLitteral bin01
  -- putStrLn $ "litt=" ++ show litt
  -- putStrLn $ "bin02=" ++ show bin02
  -- let (mode, bin02m) = parseAsBinary 1 bin01
  -- putStrLn $ "mode=" ++ show mode
  -- let (lenSub, binLenSub) = parseAsBinary 15 bin02m
  -- putStrLn $ "lenSub=" ++ show (lenSub, binLenSub)
  -- let (nbSub, binNbSub) = parseAsBinary 11 bin02m
  -- putStrLn $ "nbSub=" ++ show (nbSub, binNbSub)

  let pack = parsePacket (hexToBinary (head (lines content)))
  putStrLn $ "pack=" ++ show pack
  let (trans, _, _) = pack
  let pRes = sumVersion 0 trans
  putStrLn $ "Answer 1> " ++ show pRes

  let cRes = computeVal trans
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Version = Int
data Packet = Litteral Version Int | Operator Version Int [Packet]
  deriving (Show)

hexBinaryTable :: [(Char, String)]
hexBinaryTable = [ ('0', "0000"), ('1', "0001"), ('2', "0010"), ('3', "0011"),
                   ('4', "0100"), ('5', "0101"), ('6', "0110"), ('7', "0111"),
                   ('8', "1000"), ('9', "1001"), ('A', "1010"), ('B', "1011"),
                   ('C', "1100"), ('D', "1101"), ('E', "1110"), ('F', "1111")
                 ]

hexToBinary :: String -> String
hexToBinary line = concat (mapMaybe (`lookup` hexBinaryTable) line)


parsePacket :: String -> (Packet, Int, String)
parsePacket str = case typeId of
  4 -> (Litteral version litteral, lSize+6, lStr)
  _ -> (Operator version typeId sub, subSize+7, subStr)
  where
    (version, vStr) = parseAsBinary 3 str
    (typeId, tStr)  = parseAsBinary 3 vStr -- `debug` ("look for typeId in " ++ vStr)
    (litteral, lSize, lStr) = parseLitteral tStr
    (mode, mStr) = parseAsBinary 1 tStr
    (subBitLength, subBitStr) = parseAsBinary 15 mStr
    (subNbPack, subNbStr) = parseAsBinary 11 mStr
    (sub, subSize, subStr) = case mode of
      -- 0 -> ([], 15, subBitStr)
      0 -> (lenSubPackets, 15+lenSubSize, lenSubStr)
        where
          (lenSubPackets, lenSubSize, lenSubStr) = until (\(_, cumSize, _) -> cumSize == subBitLength)
                                                         parsePackets ([], 0, subBitStr)
      -- 1 -> ([], 11, subnbStr)
      1 -> (nbSubPackets, 11+nbSubSize, nbSubStr)
        where (nbSubPackets, nbSubSize, nbSubStr) = MU.applyN parsePackets subNbPack
                                                              ([], 0, subNbStr)
      _ -> error ("Uknown mode " ++ show mode)

parseLitteral :: String -> (Int, Int, String)
parseLitteral str = gatherBinary ([], 0, str)
  where
    gatherBinary (acc, nbBits, '1':bs) = gatherBinary (acc ++ take 4 bs, nbBits+5, drop 4 bs)
    gatherBinary (acc, nbBits, '0':bs) = (litteral, nbBits+5, drop 4 bs)
      where
        litteral = fst $ parseAsBinary (length binaryLitteral) binaryLitteral
        binaryLitteral = acc ++ take 4 bs

-- read One Packet
parsePackets :: ([Packet], Int, String) -> ([Packet], Int, String)
parsePackets (packs, size, toParse) = (packs ++ [newPack], size+packSize, leftover)
  where
    (newPack, packSize, leftover) = parsePacket toParse

parseAsBinary :: Int -> String -> (Int, String)
parseAsBinary size str = (binaryStrToDecimal $ take size str, drop size str)

binaryStrToDecimal :: String -> Int
binaryStrToDecimal bs = foldl' (\dec b -> dec * 2 + (if b == '1' then 1 else 0)) 0 bs

-- sum version, recursively
sumVersion :: Int -> Packet -> Int
sumVersion acc (Litteral ver _) = acc + ver
sumVersion acc (Operator ver _ ps) = acc + ver + sum (map (sumVersion 0) ps)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
computeVal :: Packet -> Int
computeVal (Litteral _ val) = val
computeVal (Operator _ 0 ps) = sum (map computeVal ps)
computeVal (Operator _ 1 ps) = product (map computeVal ps)
computeVal (Operator _ 2 ps) = minimum (map computeVal ps)
computeVal (Operator _ 3 ps) = maximum (map computeVal ps)
computeVal (Operator _ 5 ps) = if computeVal (head ps) > computeVal (ps !! 1)
  then 1 else 0
computeVal (Operator _ 6 ps) = if computeVal (head ps) < computeVal (ps !! 1)
  then 1 else 0
computeVal (Operator _ 7 ps) = if computeVal (head ps) == computeVal (ps !! 1)
  then 1 else 0
