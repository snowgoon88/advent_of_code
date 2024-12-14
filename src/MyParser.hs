module MyParser where

import Data.Char ( digitToInt, isDigit )
import Data.List ( isPrefixOf )
import Data.Maybe ( fromJust )
-- import qualified Data.Map as Map

data Parser a = Parser { val :: a
                       , toParse :: String
                       , stopC :: [Char] }
                deriving Show

-- pass through ' ' to read a Int ending with `Parser.stopC`
getID :: Parser Int -> String -> Parser Int
getID pInfo []  = pInfo { toParse=[] }
getID pInfo (c:cs)
  | c == ' ' = getID pInfo { toParse = cs } cs
  | isDigit c = getID pInfo { val = 10 * val pInfo + digitToInt c
                             , toParse = cs } cs
  | elem c (stopC pInfo) = pInfo { toParse = cs}
  | otherwise = error ("getID: neither ' ', digit or `Parser.stopC` (c=" ++ show c ++ ")")
  -- | otherwise = Parser { val=0, toParse=(c:cs) }

-- *****************************************************************************
-- parse a `line` begining with `label` and with an Int ID before ':'
-- ex: "Game  28:"
-- *****************************************************************************
parseLabelID :: String -> String -> Parser Int
parseLabelID label line
  | isPrefixOf label line = getID Parser { val=0, toParse=stillToParse, stopC=[':'] } stillToParse
  | otherwise = error ("parseLabelID: not starting with " ++ label ++ " (line=" ++ line ++ " )")
    where stillToParse = drop (length label) line

-- *****************************************************************************
-- parseList : parse for a list of digit seperated by ' '
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
  | otherwise = error ("parseList unexpected symbol c=[" ++ show c ++"]")
parseList ListInfo {curVal = Nothing, numList=curList } [] = ListInfo { curVal = Nothing, numList = curList, toParseL = [] }

parseList lInfo (c:cs)
  | isDigit c = parseList lInfo { curVal = updatedVal } cs
  | c == ' ' = parseList lInfo { curVal = Nothing, numList = updatedList } cs
  | otherwise = error ("parseList unexpected symbol c=[" ++ show c ++"]")
    where updatedVal = Just (10 * fromJust (curVal lInfo) + digitToInt c)
          updatedList = fromJust (curVal lInfo) : numList lInfo
parseList lInfo [] = lInfo { curVal = Nothing, numList = updatedList, toParseL=[] }
    where updatedList = fromJust (curVal lInfo) : numList lInfo

-- *****************************************************************************
-- parseLabelList : parse a line starting with `label`: and a list digit
-- *****************************************************************************
parseLabelList :: String -> String -> ListInfo
parseLabelList label line
  | isPrefixOf label line = parseList emptyListInfo (drop (length label + 1) line)
  | otherwise = error ("parseLabelList: not starting with " ++ label ++ " (line=" ++ line ++ " )")

