module Main where

import Data.List ( group, sort, sortOn, sortBy )
import Data.String.Utils ( replace, split )
import Data.Char ( digitToInt, isDigit )

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 07 - Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input07.txt"
  -- content <- readFile "Input23/test07_1.txt"

  let hands = map parseLine (lines content)
  -- print hands
  let ordered = sort hands
  -- print ordered
  let pRes = sum $ map (\tupleHandRank -> bid (fst tupleHandRank) * snd tupleHandRank)
                      (zip ordered [1..])
  putStrLn $ "Answer 1> " ++ show pRes

  let handsNew = map parseLineNew (lines content)
  -- print hands
  let orderedNew = sortBy compareNew handsNew
  -- print ordered
  let cRes = sum $ map (\tupleHandRank -> bid (fst tupleHandRank) * snd tupleHandRank)
                      (zip orderedNew [1..])
  putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

data Hand = Hand { cards :: String
                 , category :: Int
                 , bid :: Int }
instance Show Hand where
  show hand = "H: " ++ (cards hand) ++ " [" ++ show (category hand) ++ "] = " ++ show (bid hand)
instance Eq Hand where
  (==) hl hr = (==) (cards hl) (cards hr)
instance Ord Hand where
  compare hl hr
    | category hl == category hr = lexicoCardOrder (cards hl) (cards hr)
    | otherwise = compare (category hl) (category hr)

valueCard :: Char -> Int
valueCard c
  | c == 'A' = 14
  | c == 'K' = 13
  | c == 'Q' = 12
  | c == 'J' = 11
  | c == 'T' = 10
  | isDigit c = digitToInt c
  | otherwise = 0

lexicoCardOrder :: String -> String -> Ordering
lexicoCardOrder [] _ = EQ
lexicoCardOrder (l:ls) (r:rs)
  | valueCard l == valueCard r = lexicoCardOrder ls rs
  | otherwise = compare (valueCard l) (valueCard r)

-- to define the class (Five, Four, ...), first sort the cards, then group and
-- map length of groups, and sort again
classify :: String -> [Int]
classify cards = reverse $ sort ( map length (group ( sort cards )))

categorize :: [Int] -> Int
categorize [5] = 7
categorize [4, 1] = 6
categorize [3, 2] = 5
categorize [3, 1, 1] = 4
categorize [2, 2, 1] = 3
categorize [2, 1, 1, 1] = 2
categorize [1, 1, 1, 1, 1] = 1
categorize _ = 0

parseLine :: String -> Hand
parseLine line = Hand { cards = cards,
                        category = categorize (classify cards),
                        bid = bids }
  where splitted = split " " line
        cards = head splitted
        bids = read (head (tail splitted)) :: Int


-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

removeJoker :: String -> String
removeJoker = replace "J" ""

orderedCards :: String -> [String]
orderedCards = group . reverse . sortOn valueCard

bestCardToReplace :: (Char, Int) -> [String] -> Char
bestCardToReplace (c,freq) [] = c
bestCardToReplace (c,freq) (cards:cs)
  | length cards > freq = bestCardToReplace (head cards, length cards) cs
  | otherwise = bestCardToReplace (c,freq) cs

betterCombination :: String -> String
betterCombination cards = replace "J" [bestCardToReplace ('_', 0)
                                       (orderedCards $ removeJoker cards)] cards

valueCardNew :: Char -> Int
valueCardNew c
  | c == 'A' = 14
  | c == 'K' = 13
  | c == 'Q' = 12
  | c == 'J' = 0
  | c == 'T' = 10
  | isDigit c = digitToInt c
  | otherwise = -1

lexicoCardOrderNew :: String -> String -> Ordering
lexicoCardOrderNew [] _ = EQ
lexicoCardOrderNew (l:ls) (r:rs)
  | valueCardNew l == valueCardNew r = lexicoCardOrderNew ls rs
  | otherwise = compare (valueCardNew l) (valueCardNew r)

compareNew :: Hand -> Hand -> Ordering
compareNew hl hr
    | category hl == category hr = lexicoCardOrderNew (cards hl) (cards hr)
    | otherwise = compare (category hl) (category hr)

parseLineNew :: String -> Hand
parseLineNew line = Hand { cards = cards,
                        category = categorize (classify cardsBest),
                        bid = bids }
  where splitted = split " " line
        cards = head splitted
        bids = read (head (tail splitted)) :: Int
        cardsBest = betterCombination cards
