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
import Data.String.Utils (split)
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
import Control.Monad ( replicateM )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', intersect, union
import Data.List (intersect, union, delete, sort, sortOn)
-- ****** Data.List.Extra: splitOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
import qualified Control.Monad.Trans.State as St
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
  putStrLn "** Advent 2020 - Day 21 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input21.txt"
  -- content <- readFile "Input20/test21_1.txt"

  let bothMaps = foldl opBuildAlMap (Map.empty, Map.empty) (lines content)
  putStrLn $ "alMap=" ++ show bothMaps

  let allIngredients = Set.fromList (Map.keys (fst bothMaps))
  let mustBeAlergens = Set.fromList (foldl union [] $ Map.elems (snd bothMaps))
  let cannotBeAlergens = Set.difference allIngredients mustBeAlergens

  -- putStrLn $ "allIng=" ++ show allIngredients
  -- putStrLn $ "mustAl=" ++ show mustBeAlergens
  -- putStrLn $ "cannotBe=" ++ show cannotBeAlergens

  let pRes = countIngredients (fst bothMaps) cannotBeAlergens
  putStrLn $ "Answer 1> " ++ show pRes

  let cand00 = extractCandidate (snd bothMaps)
  putStrLn $ "cand01: " ++ show cand00
  let solCan01 = addSolutions ([], cand00)
  putStrLn $ "solCan01: " ++ show solCan01
  let allMatch01 = assignAlergen cand00
  putStrLn $ "allMatch01=" ++ show allMatch01

  let assignEx2 = assignExample cand00
  putStrLn $ "assignEx3:  " ++ show assignEx2

  let assignment = findAssignemt cand00
  putStrLn $ "assignment: " ++ show assignment
  -- hopefully, only ONE assignment. Sort on Alergen order
  let orderedAssignment = sortOn snd (head assignment)
  putStrLn $ "OrderedAssignment: " ++ show orderedAssignment

  let cRes = init $ foldl (\res (i, _) -> res ++ i ++ ",") "" orderedAssignment
  putStrLn $ "Answer 2> " ++ show cRes


  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Ingredient = String
type Alergen = String

parseFood :: String -> ([Ingredient], [Alergen])
parseFood line = (words $ head food, split ", " allergens)
  where
    food = split "(contains " line
    allergens = init (food !! 1)

-- an ingredient can only have an allergene if it is present everytime the allergene
-- is present
-- Map: al -> list of (Set of ingredient)
-- for an all, compute the intersection
-- then th union of intersections ? and take the complementary ?
type AlMap = Map.Map Alergen [Ingredient]
-- TODO why ICountMap needed
-- need also to list (and count) ingredient apparition
type ICountMap = Map.Map Ingredient Int

-- build the Maps from the file
opBuildAlMap :: (ICountMap, AlMap) -> String -> (ICountMap, AlMap)
opBuildAlMap (iMap, aMap) line = ( foldl (\m i -> Map.insertWith (+) i 1 m) iMap ingredients
                                 , foldl (\m a -> Map.insertWith intersect a ingredients m) aMap alergens )
  where
    (ingredients, alergens) = parseFood line

-- count the presence of Ingredients
countIngredients :: (Num b, Ord k) => Map.Map k b -> Set.Set k -> b
countIngredients iMap iSet = Map.foldr (+) 0 (Map.restrictKeys iMap iSet)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- Maintain an ordered list of ([candiates ingredient], alergen)
-- ordered by the number of candidates
-- Remove all entry with only one candidate.
type Match = (Ingredient, Alergen)
type Solution = [Match]
type Candidates = [([Ingredient], Alergen)]

-- extract Candidates from aMap
extractCandidate :: AlMap -> Candidates
extractCandidate aMap = map (\(a,b) -> (sort b,a)) $ Map.toList aMap

-- add all constrained choice (only 1 ingredient) of Candidates
addSolutions :: (Solution, Candidates) -> (Solution, Candidates)
addSolutions (sol, candidates) = foldr opAdd (sol, []) candidates
  where
    opAdd (i, a) (accSol, accCandidate) = if length i == 1
      then ((head i, a):accSol, accCandidate)
      else (accSol, (i, a):accCandidate)

-- assign on alergen and update the candidates
assignAlergen :: Candidates -> [(Match, Candidates)]
assignAlergen [] = []
assignAlergen ((is, a):cs) = map opMatch is
  where
    opMatch chosen = ((chosen, a), opPrune cs chosen)

    opPrune :: Candidates -> Ingredient -> Candidates
    opPrune [] _ = []
    opPrune ((iList, alergen):xs) ing = (delete ing iList, alergen) : opPrune xs ing

-- if one of the ingredient List in= []
stopAssignment :: Candidates -> Bool
stopAssignment [] = True
stopAssignment (([], _):_)  = False
stopAssignment ((_,  _):cs) = stopAssignment cs

-- use StatT with the List Monad to store all the possibilities
-- St.evalStateT :: Monad m => St.StateT s m a -> s -> m a
-- St.runStateT :: St.StateT s m a -> s -> m (a, s)
-- assignExample :: Candidates -> [[Match]]
assignExample :: Candidates -> [([Match], Candidates)]
assignExample candidates = flip St.runStateT candidates $ do
  m1 <- St.StateT assignAlergen
  m2 <- St.StateT assignAlergen
  -- m3 <- St.StateT assignAlergen
  return [m1, m2]

findssignemt :: [([Ingredient], Alergen)] -> [[Match]]
findssignemt candidates = flip St.evalStateT candidates $ do
  replicateM (length candidates) (St.StateT assignAlergen)
-- findAssignemt candidates = flip St.evalState candidates (replicateM (length candidates) (St.StateT assignAlergen))
