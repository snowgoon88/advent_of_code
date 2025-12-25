{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoOverloadedStrings #-}
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
-- ****** Data.Foldable ( foldl' )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
import qualified Data.Bits as B
import qualified Data.Sequence as S
import qualified Data.SBV as SBV
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
import Data.List (transpose)
import Data.Foldable ( forM_, notElem, toList )
-- ****** Data.List.Extra: splitOn
import Data.List.Extra ( splitOn )
-- import Data.List.Split ( splitOn )
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

-- *****************************************************************************************
-- *****************************************************************************************
{- Essai d'utilisation du module Data.SBV pour résoudre un problem linéaire entier.
   Tiré de Advent Of Code 2025, jour 10.

   Bon, ne marche pas car "z3", le solveur par défaut utilisé dans ce cas là par
   SBV n'est pas installé.
-}
-- *****************************************************************************************
-- *****************************************************************************************
exInput :: String
exInput = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2025 - Day 10bis Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  -- putStrLn $ "Answer 1> " ++ show pRes

  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
-- A button Matrix is a Sequence of Sequence of Bool
type BMat = S.Seq (S.Seq Bool)
type Vec = S.Seq Int

-- transpose a Seq (Seq a) using transpose on List
seqTranspose :: S.Seq (S.Seq a) -> S.Seq (S.Seq a)
seqTranspose = S.fromList . fmap S.fromList . transpose . fmap toList . toList

-- create a List of size pad from integer nb
toBitsPadded :: Int -> Int -> [Bool]
toBitsPadded pad nb = go pad nb
  where
    go :: Int -> Int -> [Bool]
    go p 0 = replicate p False
    go p n = odd n : go (p - 1) (n B..>>. 1)


toBMat :: Int -> [Int] -> BMat
toBMat pad ns = seqTranspose m0
  where
    m0 = S.fromList $ fmap (S.fromList . toBitsPadded pad) ns

-- remove [](){} and split on SPACE
parseSections :: String -> S.Seq String
parseSections = S.fromList . words . filter ( `notElem` "[](){}" )

fromBitIxs :: (B.Bits a, Foldable f) => f Int -> a
fromBitIxs =
  -- use Ior monoid (Inclusive OR, meaning regular OR) to compute a mask
  -- B.bit i  sets the i-th bit
  B.getIor . foldMap (\i -> if i >= 0 then B.Ior (B.bit i) else B.Ior B.zeroBits)

-- create an Int made from bit mask with 1 at button position
parseButton :: String -> Int
-- using >>> from Control.Arrow, could write as:
--            splitOn "," >>> fmap read >>> toList >>> fromBitIxs
parseButton = fromBitIxs . toList . fmap read . splitOn ","

-- list of StrCounts to Seq of IntCounts
parseCounts :: String -> S.Seq Int
parseCounts = S.fromList . fmap read . splitOn ","

-- list of [which buttons increases the i-th counter]_i, count_i
parseSystem :: String -> (BMat, Vec)
parseSystem s = (toBMat (length counts) (toList buttons), counts)
  where
    (buttons, counts) =
      case parseSections s of
        (_ S.:<| sb) S.:|> sc -> (parseButton <$> sb, parseCounts sc)
        _ -> undefined
{-
sbv: SMT Based Verification: Symbolic Haskell theorem prover using SMT solving.

Documentation.SBV.Examples.Optimization.LinearOpt
Simple linear optimization example, as found in operations research texts.
Synopsis
Documentation

problem :: ConstraintSet

Taken from http://people.brunel.ac.uk/~mastjjb/jeb/or/morelp.html

    maximize 5x1 + 6x2

    subject to
        x1 + x2 <= 10
        x1 - x2 >= 3
        5x1 + 4x2 <= 35
        x1 >= 0
        x2 >= 0

problem :: ConstraintSet
problem = do [x1, x2] <- mapM sReal ["x1", "x2"]

             constrain $ x1 + x2 .<= 10
             constrain $ x1 - x2 .>= 3
             constrain $ 5*x1 + 4*x2 .<= 35
             constrain $ x1 .>= 0
             constrain $ x2 .>= 0

             maximize "goal" $ 5 * x1 + 6 * x2

>>> optimize Lexicographic problem
Optimal model:
  x1   =  47 % 9 :: Real
  x2   =  20 % 9 :: Real
  goal = 355 % 9 :: Real

-}

-- *****************************************************************************
-- ERROR: could work, but NEED z3 solver !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- *****************************************************************************

varNames :: Int -> [String]
varNames n = fmap (\i -> "x" ++ show i) [0 .. n - 1]
-- make a SBV system to solve
mkSystem :: BMat -> Vec -> ([String], SBV.ConstraintSet)
mkSystem ma b = (vars,) $ do
  -- use Integer Variables
  x <- S.fromList <$> mapM SBV.sInteger vars
  -- all x_i must be >= 0
  forM_ x $ \xi -> SBV.constrain $ xi SBV..>= 0
  forM_ (ma `S.zip` b) $ \(c, t) -> do
    let vars = [xi | (xi, b) <- toList (x `S.zip` c), b]
    SBV.constrain $ sum vars SBV..== SBV.literal (fromIntegral t)
  SBV.minimize "goal" $ sum x
  where
    mt    = seqTranspose ma
    nVars = length mt
    vars  = varNames nVars

solveSystem :: BMat -> Vec -> IO Vec
solveSystem ma b = do
  let (vars, sys) = mkSystem ma b
  res <- SBV.optimize SBV.Lexicographic sys
  case res of
    SBV.LexicographicResult r@SBV.Satisfiable{} -> do
      let vals = traverse (flip (SBV.getModelValue @SBV.SMTResult @Integer) r) vars
      case vals of
        Just vs -> pure . S.fromList . fmap fromIntegral $ vs
        _ -> undefined
    _ -> undefined

solve line = res
  where
    (ma, v) = parseSystem line
    -- (vars, sys) = mkSystem ma v
    res = solveSystem ma v
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

