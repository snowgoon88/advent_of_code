{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid )
-- import Data.List ( sortOn, groupBy ) --, find ) --group ) --, sort, , sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split ) --replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
import Debug.Trace ( trace )
-- import Numeric ( readHex )

l1 = "px{a<2006:qkq,m>2090:A,rfg}"
e1 = "{x=787,m=2655,a=1222,s=2876}"
code1 = parseLine l1
ex1 = parsePart e1

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 19 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input19.txt"
  -- content <- readFile "Input23/test19_1.txt"

  let rules = map parseLine (filter (\l -> l /= "" && head l /= '{') (lines content))
  let parts = map parsePart (filter (\l -> l /= "" && head l == '{') (lines content))
  let ruleMap = Map.fromList rules
  -- print $ "rules=" ++ show rules
  -- print $ "parts=" ++ show parts

  -- let ex1 = parsePart e1
  -- print $ "ex1=" ++ show ex1
  -- let exFlow = applyRules ruleMap "in" ex1
  -- print $ "exFlow=" ++ show exFlow

  let flow = map (applyRules ruleMap "in") parts
  -- print $ "flow=" ++ show flow

  let pRes = sum flow
  putStrLn $ "Answer 1> " ++ show pRes

  let code1 = parseLine l1
  let ex1 = parsePart e1
  print $ "code1" ++ show code1
  print $ "ex1=" ++ show ex1
  let fullIntervale = replicate 4 (1,4000)
  let ranges = intervaleRules ruleMap [("in", fullIntervale)]
  -- print $ "ranges=" ++ show ranges

  let cRes = sum ranges
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Operator = Inf | Sup
  deriving Show
data Step = Step { field :: Int
                 , op :: Operator
                 , threshold :: Int
                 , dest :: String }
  deriving Show

data Code = Code { steps :: [Step]
                 , uncondition :: String}
  deriving Show

type RuleMap = Map.Map String Code

type Example = [Int]

idField 'x' = 0
idField 'm' = 1
idField 'a' = 2
idField 's' = 3

parseStep :: String -> Step
parseStep (f:symbol:ps) = Step (idField f) op thres label
  where
    tokens = split ":" ps
    thres = read (head tokens)
    label = head( drop 1 tokens )
    op = case symbol of '<' -> Inf
                        '>' -> Sup

parseCode :: String -> Code
parseCode code = Code (map parseStep (init tokens)) (last tokens)
  where
    tokens = split "," (init code)

parseLine :: String -> (String, Code)
parseLine line = (head tokens, parseCode (head (drop 1 tokens)))
  where
    tokens = split "{" line

parseCat :: String -> (String, Int)
parseCat catStr = (head tokens, read (tokens !! 1))
  where tokens = split "=" catStr

parsePart :: String -> Example
parsePart partStr = map snd categories
  where
    tokens = split "," (tail (init partStr))  -- remove "{}"
    categories = map parseCat tokens

-- *********************************************************************** RULES
applyRules :: RuleMap -> String -> Example -> Int
applyRules ruleMap label ex =
  case applyCode ex (steps code) (uncondition code) of
    "A" -> rating ex
    "R" -> 0
    nextLabel -> applyRules ruleMap nextLabel ex
  where code = ruleMap Map.! label

applyCode :: Example -> [Step] -> String -> String
applyCode ex [] defLabel = defLabel
applyCode ex (c:cs) defLabel = case applyStep ex c of
  Just label -> label
  Nothing -> applyCode ex cs defLabel

applyStep :: Example -> Step -> Maybe String
applyStep ex (Step f Inf thres dest)
  | ex !! f < thres = Just dest
  | otherwise       = Nothing
applyStep ex (Step f Sup thres dest)
  | ex !! f > thres = Just dest
  | otherwise       = Nothing

rating :: Example -> Int
rating ex = sum ex

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
type Intervale = [(Int, Int)]

splitIntervale :: Intervale -> Int -> Int -> (Maybe Intervale, Maybe Intervale)
splitIntervale intervale f thres
  | thres > high = (Just (startIntervale ++ [(low, high)] ++ endIntervale), Nothing)
  | thres <= low = (Nothing, Just (startIntervale ++ [(low, high)] ++ endIntervale))
  | otherwise    = (Just (startIntervale ++ [(low,thres-1)] ++ endIntervale),
                    Just (startIntervale ++ [(thres, high)] ++ endIntervale))
  where
    startIntervale = take f intervale
    endIntervale = drop (f+1) intervale
    (low, high) = intervale !! f

intervaleStep :: Intervale -> Step -> (Maybe (String, Intervale), Maybe Intervale)
intervaleStep inter (Step f Inf thres dest) =
  case splitIntervale inter f thres of
    (Just i1, Just i2) -> (Just (dest, i1), Just i2)
    (Just i1, Nothing) -> (Just (dest, i1), Nothing)
    (Nothing, Just i2) -> (Nothing, Just i2)
intervaleStep inter (Step f Sup thres dest) =
  case splitIntervale inter f (thres+1) of
    (Just i1, Just i2) -> (Just (dest, i2), Just i1)
    (Just i1, Nothing) -> (Nothing, Just i1)
    (Nothing, Just i2) -> (Just (dest, i2), Nothing)
-- DEBUG version with trace
intervaleStepT inter (Step f Inf thres dest) =
  case splitIntervale inter f thres of
    (Just i1, Just i2) -> debug (Just (dest, i1), Just i2)
    (Just i1, Nothing) -> debug (Just (dest, i1), Nothing)
    (Nothing, Just i2) -> debug (Nothing, Just i2)
  where debug a = trace ("  Step " ++ show inter ++ " " ++ show f ++ "<" ++ show thres ++ " -> " ++ show a) a
intervaleStepT inter (Step f Sup thres dest) =
  case splitIntervale inter f (thres+1) of
    (Just i1, Just i2) -> debug (Just (dest, i2), Just i1)
    (Just i1, Nothing) -> debug (Nothing, Just i1)
    (Nothing, Just i2) -> debug (Just (dest, i2), Nothing)
  where debug a = trace ("  Step " ++ show inter ++ " " ++ show f ++ ">" ++ show thres ++ " -> " ++ show a) a

intervaleCode :: Intervale -> [Step] -> String -> [(String, Intervale)]
intervaleCode inter [] defLabel = [(defLabel, inter)]
intervaleCode inter (c:cs) defLabel = case intervaleStep inter c of
  (Just next1, Nothing) -> [next1]
  (Nothing, Just i2) -> intervaleCode i2 cs defLabel
  (Just next1, Just i2) -> next1 : intervaleCode i2 cs defLabel

intervaleRules :: RuleMap -> [(String, Intervale)] -> [Int]
intervaleRules _ [] = []
intervaleRules ruleMap ((label, inter):is)
  | label == "A" = intervaleRating inter : intervaleRules ruleMap is
  | label == "R" = intervaleRules ruleMap is
  | otherwise = intervaleRules ruleMap (intervaleCode inter (steps code) (uncondition code)
                                        ++ is)
  where
    code = ruleMap Map.! label

intervaleRating :: Intervale -> Int
intervaleRating inter = foldr (\f prod -> prod * (snd f - fst f + 1)) 1 inter

-- intervaleStep :: Intervale -> Step -> [(String, Intervale)]
-- interaleStep in (Step f Inf thres dest) = (dest, )
