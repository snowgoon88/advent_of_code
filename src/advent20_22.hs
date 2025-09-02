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
import MyUtils as MU

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
-- debug :: c -> String -> c
-- debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 22 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input22.txt"
  -- content <- readFile "Input20/test22_1.txt"

  let decks = map readDeck (MU.groupLines (lines content))
  let startGame = (head decks, decks !! 1)
  -- putStrLn $ "decks=" ++ show startGame

  let finalDecks = run startGame oneRound
  putStrLn $ "end=" ++ show finalDecks

  let pRes = scoreDeck (bestDeck finalDecks)
  putStrLn $ "Answer 1> " ++ show pRes

  let finalGame = gameRound Map.empty startGame
  putStrLn $ "final=" ++ show finalGame
  let cRes = case finalGame of
        Left g  -> scoreDeck g
        Right g -> scoreDeck g
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Deck = [Int]

readDeck :: [String] -> Deck
readDeck fileLines = map read (tail fileLines)

oneRound :: (Deck, Deck) -> Maybe (Deck, Deck)
oneRound ([],_) = Nothing
oneRound (_,[]) = Nothing
oneRound (a:as, b:bs)
  | a > b     = Just (as ++ [a, b], bs)
  | otherwise = Just (as, bs ++ [b, a])

run :: a -> (a -> Maybe a) -> a
run s f = case f s of
  Just ns -> run ns f
  Nothing -> s

bestDeck :: (Deck, Deck) -> Deck
bestDeck (d, []) = d
bestDeck ([], d) = d
bestDeck (d, _) = d


scoreDeck :: Deck -> Int
scoreDeck deck = sum $ zipWith (*) [1..] (reverse deck)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- need a Map (Deck, Deck) -> Bool to know if configuration already seen
type DeckMap = Map.Map (Deck, Deck) Bool

-- If player1 wins, return Left Deck, otherwise Right Deck
gameRound :: DeckMap -> (Deck, Deck) -> Either Deck Deck
gameRound _ (a, []) = Left a -- `debug` "Player1 wins"
gameRound _ ([], b) = Right b -- `debug` "Player2 wins"
gameRound dMap (a:as, b:bs) = case Map.lookup (a:as, b:bs) dMap of
  Just _ -> Left (a:as)  -- 1 Wins
  Nothing -> if a <= length as && b <=length bs
               then gameRound newMap (resolve subGame) -- `debug` ("subGame with " ++ niceGame)
               else gameRound newMap (resolve winner) -- `debug` ("regular with " ++ niceGame)
    where
      newMap = Map.insert (a:as, b:bs) True dMap
      subGame = gameRound Map.empty (take a as, take b bs)
      winner = if a > b then Left (a:as)
                        else Right (b:bs)
      resolve :: Either Deck Deck -> (Deck, Deck)
      resolve (Left _)  = (as ++ [a, b], bs)
      resolve (Right _) = (as, bs ++ [b, a])

      -- niceGame = show (a:as) ++ " vs " ++ show (b:bs)
