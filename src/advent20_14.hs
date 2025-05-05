{-# LANGUAGE TupleSections #-}
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
import Data.Char (digitToInt)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
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


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 14 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input14.txt"
  -- content <- readFile "Input20/test14_1.txt"
  -- content <- readFile "Input20/test14_2.txt"

  let cmds = map parseCmd (lines content)
  -- putStrLn $ "cmds=" ++ show cmds

  let term = run (C "" Map.empty) cmds
  -- putStrLn $ "term=" ++ show term

  let pRes = sumMem term

  putStrLn $ "Answer 1> " ++ show pRes

  let termAdv = runAdv (C "" Map.empty) cmds
  -- putStrLn $ "termAdv=" ++ show termAdv
  let cRes = sumMem termAdv
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
-- Computer Memory
type Memory = Map.Map Int String
data Computer = C { cmask :: String
                  , cmem :: Memory }
  deriving (Show)

data Command = Mask String | Mem Int String
  deriving (Show)

parseCmd :: String -> Command
parseCmd ('m':'a':'s':'k':' ':'=':' ':ms) = Mask ms
parseCmd str = Mem add value
  where
    add = read (takeWhile (/=']') $ drop 4 str)
    value = asString (read (drop 2 $ dropWhile (/='=') str)) (2^35)

asString :: Int -> Int -> String
asString val 1 = show val
asString val power = digit : asString newVal (div power 2)
  where
    (m, newVal) = divMod val power
    digit = if m > 0 then '1' else '0'

asInt :: String -> Int
asInt code = op 1 $ reverse code
  where
    op _ [] = 0
    op val ('0':xs) = op (2*val) xs
    op val ('1':xs) = val + op (2*val) xs

run :: Computer -> [Command] -> Computer
run comp prog = foldl exec comp prog

exec :: Computer -> Command -> Computer
exec comp (Mask m) = comp { cmask = m }
exec comp (Mem add sVal) = comp { cmem = newMap }
  where
    newMap = Map.insert add (applyMask (cmask comp) sVal) (cmem comp)

applyMask :: String -> String -> String
applyMask mask sVal = map (uncurry combineBit) (zip mask sVal)

combineBit :: Char -> Char -> Char
combineBit 'X' cMask = cMask
combineBit '1' _     = '1'
combineBit '0' _     = '0'
combineBit _   _     = '?'

sumMem :: Computer -> Int
sumMem comp = sum (map asInt (Map.elems (cmem comp)))

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- combine mask with char from encoded address
genAdd :: Char -> Char -> String
genAdd 'X' _ = ['0', '1']
genAdd '0' c = [c]
genAdd '1' _ = ['1']

-- This using the exaustive search using the list Monad
-- to generate all addresses.
{--
      do x <- "abc"
         y <- "abc"
         z <- "abc"
         return [x,y,z]
["aaa","aab","aac","aba","abb" ... ]
https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html
https://blog.jle.im/entry/wolf-goat-cabbage-the-list-monadplus-logic-problems.html
--}
allAdd :: String -> String -> [String]
allAdd mask memStr = mapM (uncurry genAdd) (zip mask memStr)

execAdv :: Computer -> Command -> Computer
execAdv comp (Mask m) = comp { cmask = m }
execAdv comp (Mem add sVal) = comp { cmem = newMap }
  where
    sAdd = asString add (2^35)
    addressesStr = allAdd (cmask comp) sAdd
    newMap = foldl opAdd (cmem comp) addressesStr
      where
        opAdd mem addr = Map.insert (asInt addr) sVal mem

runAdv :: Computer -> [Command] -> Computer
runAdv comp prog = foldl execAdv comp prog
