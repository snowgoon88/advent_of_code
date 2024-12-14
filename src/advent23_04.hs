module Main where

import qualified MyParser as MP
import Data.Char ( digitToInt, isDigit )
import Data.Maybe ( fromJust )

tl1 = "Card   1: 82 15 37 75 85 51 99 18 17  2 |  8 17 54 14 18 99  4 85 51 49 91 15 82 24 75 25 69 61 52 58 37 87  2 22 28"
tl2 = "Card   2: 67 26 84 63 48 73 36 94 89 65 | 36 94 96 65 89 87 12 26 81 82 77 99 40 63  6 73 55 48 10 69 59 78 24 67 84"

c1 = parseCard tl1
c2 = parseCard tl2

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 04 - Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input04.txt"
  -- content <- readFile "Input23/test04_1.txt"

  let cards = map parseCard (lines content)
  -- putStrLn $ "cards=" ++ show cards
  let scores = (map (scoreCard . parseCard) (lines content))
  -- putStrLn $ "scores=" ++ show scores
  let pRes = sum scores
  -- let pRes = foldr parseLine emptyInfo (lines content)
  putStrLn $ "Answer 1> " ++ show pRes

  let nbWins = map (length . winningNb) cards
  let nbCards = take (length cards) (repeat 1)
  let cRes = sum $ stack [] nbCards nbWins
  putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

data ListInfo = ListInfo { curVal :: Maybe Int
                           , numList :: [Int]
                           , toParseL :: String }
  deriving Show
emptyListInfo = ListInfo { curVal=Nothing, numList=[], toParseL="" }

parseList :: ListInfo -> String -> ListInfo
parseList ListInfo {curVal = Nothing, numList=curList } (c:cs)
  | isDigit c = parseList ListInfo { curVal = Just (digitToInt c), numList = curList, toParseL = cs } cs
  | c == ' ' = parseList ListInfo { curVal = Nothing, numList = curList, toParseL = cs } cs
  | c == '|' = ListInfo { curVal=Nothing, numList = curList, toParseL = cs }
  | otherwise = error ("parseList unexpected symbol c=[" ++ show c ++"]")
parseList ListInfo {curVal = Nothing, numList=curList } [] = ListInfo { curVal = Nothing, numList = curList, toParseL = [] }

parseList lInfo (c:cs)
  | isDigit c = parseList lInfo { curVal = updatedVal } cs
  | c == ' ' = parseList lInfo { curVal = Nothing, numList = updatedList } cs
  | c == '|' = lInfo { curVal = Nothing, toParseL = cs }
  | otherwise = error ("parseList unexpected symbol c=[" ++ show c ++"]")
    where updatedVal = Just (10 * fromJust (curVal lInfo) + digitToInt c)
          updatedList = fromJust (curVal lInfo) : numList lInfo
parseList lInfo [] = lInfo { curVal = Nothing, numList = updatedList, toParseL=[] }
    where updatedList = fromJust (curVal lInfo) : numList lInfo


data Card = Card { idC :: Int
                 , listWin :: [Int]
                 , listDraw :: [Int] }
            deriving Show

parseCard :: String -> Card
parseCard line = Card { idC = (MP.val idParsed)
                      , listWin = (numList winParsed)
                      , listDraw = (numList drawParsed) }
  where idParsed = MP.parseLabelID "Card " line
        winParsed = parseList emptyListInfo (MP.toParse idParsed)
        drawParsed = parseList emptyListInfo (toParseL winParsed)

-- score is 1 to the power of number of winning numbers
scoreCard card = pow2 $ length (winningNb card)

-- check number of winners
winningNb :: Card -> [Int]
winningNb card = filter (\e -> elem e (listWin card)) (listDraw card)

pow2 :: Int -> Int
pow2 0 = 0
pow2 1 = 1
pow2 n = 2 * pow2 (n-1)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- Alternative way, add to nb present according to winningNb
-- add `val` to first `nb` elements of list
pile :: Int -> Int -> [Int] -> [Int]
pile val 0 values = values
pile _ _ [] = []
pile val nb values = (val + head values) : pile val (nb-1) (tail values)

-- stack alreadyStacked stillToStack winingNbs -> list of instances of each card
stack :: [Int] -> [Int] -> [Int] -> [Int]
stack stacked [] _ = stacked
stack stacked (c:cs) (w:ws) = stack (c:stacked) (pile c w cs) ws
