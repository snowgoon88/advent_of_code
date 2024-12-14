module Main where

import Data.Char ( digitToInt, isDigit )

tl1 = "......644............612.......254..638..............802.................................118.....................................317.691...."
tl2 = ".....*......321..176....+........&...=...906........*.......=518................994..938.*.....579....35....155...........320...........$..."
pt1 = parseAny emptyInfo (tl1, [0..])

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 03 - Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input03.txt"
  -- content <- readFile "Input23/test03_1.txt"

  let pRes = foldr parseLine emptyInfo (lines content)
  putStrLn $ "Answer 1> " ++ show (valSum pRes)

  let allSymb = foldr opSymbols [] (zip (lines content) [0..])
  -- putStrLn $ show allSymb
  let allNum = foldr opNum ParseNumInfo { curNum = emptyNum, numList = [] } (zip (lines content) [0..])
  -- putStrLn $ show allNum
  let allNeigbors = map (\s -> filter (isNeighbor s) (numList allNum)) allSymb
  -- putStrLn $ show allNeigbors
  let allValidNeighbors = filter ((==) 2 . length) allNeigbors
  -- putStrLn $ show allValidNeighbors
  let validRes = sum (map prodNeighbors allValidNeighbors)

  putStrLn $ "Answer 2> " ++ show validRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Amount = Amount { val :: Int
                     , start :: Int
                     , end :: Int
                     , used :: Bool }
              deriving Show

amountInRange :: Int -> Amount -> Bool
amountInRange pos amount = (start amount - 1) <= pos && pos <= (end amount + 1)

type SymbolPos = [Int]

data ParseInfo = ParseInfo { valSum :: Int
                           , prevAmount :: [Amount]
                           , prevSymbol :: SymbolPos
                           , curAmount :: [Amount]
                           , curSymbol :: SymbolPos }
                 deriving Show

emptyInfo = ParseInfo { valSum=0, prevAmount=[], prevSymbol=[]
                      , curAmount=[], curSymbol=[] }

addAmount :: ParseInfo -> Amount -> ParseInfo
addAmount pInfo amount = pInfo { valSum = valSum pInfo + val amount  }

hasSymbolAt :: Int -> SymbolPos -> Bool
hasSymbolAt pos (p:ps)
  | pos == p  = True
  | otherwise = hasSymbolAt pos ps
hasSymbolAt pos [] = False

hasSymbolIn :: Int -> Int -> SymbolPos -> Bool
hasSymbolIn start end (p:ps)
  | start <= p && p <= end = True
  | otherwise = hasSymbolIn start end ps
hasSymbolIn start pos [] = False

-- digit -> | cur_nb => cur_nb <- cur_nb * 10 + digit; pos_fin += 1
--          | no     => cur_nb <- digit, pos_start = pos_fin = idx
--          SI symbole juste avant => nb_curr.used ET sum += cur_nb
--          SI symbol prec entre start-1, end+1 => add
-- symbole  SI adj cur_nb, sum += cur_nb ET nb_curr.used
--          SI nb.start/end dans pos_symbole => ADD
--          SINON pos dans prec
-- new_line => prec devient line actuelle

parseNb :: ParseInfo -> Amount -> ([Char], [Int]) -> ParseInfo
parseNb pInfo amount (c:cs, idx:is)
  -- keep reading a number
  | isDigit c = parseNb pInfo amountUpdated (cs, is)
  -- endOfNumber, symbol in Front : update valSum
  | c == '.' && hasSymbolAt (start amount - 1) (curSymbol pInfo) =
    parseAny (addAmount pInfo amount) (cs, is)
  -- endOfNumber, prevSymbol in Range : update valSum
  | c == '.' && hasSymbolIn (start amount - 1) (end amount + 1) (prevSymbol pInfo) =
    parseAny (addAmount pInfo amount) (cs, is)
  -- endOfNumber, no prevSymbol in Range : update curAmount
  | c == '.' = parseAny pInfo { curAmount = amount:(curAmount pInfo) } (cs, is)
  -- symbol => check prevAmount, update curSymbol
  | otherwise = parseAny (addAmount updatedPInfo { curSymbol = idx:(curSymbol pInfo) } amount) (cs, is)
  -- TODO check also prevAmount !!!! as we have a symbol
  where amountUpdated = amount { val = 10 * (val amount) + digitToInt c
                               , end = idx }
        updatedPInfo = checkWithSymbol pInfo idx
parseNb pInfo amount ([], idx:is)
  -- endOfNumber, symbol in Front : update valSum
  | hasSymbolAt (start amount - 1) (curSymbol pInfo) =
    addAmount pInfo amount
  -- endOfNumber, prevSymbol in Range : update valSum
  | hasSymbolIn (start amount - 1) (end amount + 1) (prevSymbol pInfo) =
    addAmount pInfo amount
  -- endOfNumber, no prevSymbol in Range : update curAmount
  | otherwise = pInfo { curAmount = amount:(curAmount pInfo) }

parseAny :: ParseInfo -> ([Char], [Int]) -> ParseInfo
parseAny pInfo (c:cs, idx:is)
  | isDigit c = parseNb pInfo newAmount (cs, is)
  | c == '.' = parseAny pInfo (cs, is)
  | otherwise = parseAny updatedPInfo { curSymbol=idx:(curSymbol pInfo)} (cs,is)
  where newAmount = Amount { val = digitToInt c, start=idx, end=idx, used=False }
        updatedPInfo = checkWithSymbol pInfo idx
parseAny pInfo _ = pInfo

checkWithSymbol :: ParseInfo -> Int -> ParseInfo
checkWithSymbol pInfo pos = pInfo { valSum = newSum, prevAmount = amountToKeep}
  where amoutToAdd = filter (amountInRange pos) (prevAmount pInfo)
        amountToKeep = filter (not . amountInRange pos) (prevAmount pInfo)
        newSum = valSum pInfo + sum (map val amoutToAdd)

-- foldr :: (a -> b -> b) -> b -> t a -> b
parseLine line pInfo = parseAny emptyInfo { valSum = valSum pInfo
                                       , prevAmount = curAmount pInfo
                                       , prevSymbol = curSymbol pInfo }
                             (line, [0..])

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
data SymbolInfo = SymbolInfo { lineS :: Int, idxS :: Int }
  deriving Show

parseForSymbol :: [SymbolInfo] -> ([Char], [Int]) -> Int -> [SymbolInfo]
parseForSymbol sList (c:cs, ipos:is) iline
  | c == '*' = parseForSymbol (newSymb:sList) (cs, is) iline
  | otherwise = parseForSymbol sList (cs, is) iline
    where newSymb = SymbolInfo { lineS=iline, idxS=ipos }
parseForSymbol sList ([], _) _ = sList


opSymbols :: (String, Int) -> [SymbolInfo] -> [SymbolInfo]
opSymbols (line, iline) sList = (parseForSymbol [] (line, [0..]) iline) ++ sList

data NumInfo = NumInfo { valN :: Int
                       , lineN:: Int, startN :: Int, endN :: Int }
  deriving Show

emptyNum = NumInfo { valN = 0, lineN = 0, startN = 0, endN = 0 }

addToNum :: NumInfo -> Int -> NumInfo
addToNum num digit = num { valN = 10 * (valN num) + digit
                         , endN = (endN num) + 1}

data ParseNumInfo = ParseNumInfo { curNum :: NumInfo, numList :: [NumInfo] }
  deriving Show

parseForNum :: ParseNumInfo -> ([Char], [Int]) -> Int -> ParseNumInfo
parseForNum pnInfo (c:cs, p:ps) iline
  -- keep reading a number
  | isDigit c = parseForNum pnInfo { curNum = updatedNum } (cs, ps) iline
  -- endOfNum
  | otherwise = parseForAny pnInfo { curNum = emptyNum, numList = updatedList } (cs, ps) iline
  -- TODO check also prevAmount !!!! as we have a symbol
  where updatedNum = addToNum (curNum pnInfo) (digitToInt c)
        updatedList = (curNum pnInfo):(numList pnInfo)
parseForNum pnInfo ([], p:ps) iline = pnInfo { curNum = emptyNum, numList = updatedList }
  where updatedList = (curNum pnInfo):(numList pnInfo)


parseForAny :: ParseNumInfo -> ([Char], [Int]) -> Int -> ParseNumInfo
parseForAny pnInfo (c:cs, p:ps) iline
  | isDigit c = parseForNum pnInfo { curNum = updatedNum } (cs, ps) iline
  | otherwise = parseForAny pnInfo (cs, ps) iline
    where updatedNum = NumInfo { valN = digitToInt c
                               , lineN = iline
                               , startN = p
                               , endN = p }
parseForAny pnInfo ([], _) _ = pnInfo

isNeighbor :: SymbolInfo -> NumInfo -> Bool
isNeighbor sInfo numInfo = abs (lineS sInfo - lineN numInfo) <= 1 &&
                           (startN numInfo - 1) <= (idxS sInfo) &&
                           (idxS sInfo) <= (endN numInfo + 1)

opNum (line, iline) numInfo = parseForAny numInfo  (line, [0..]) iline

prodNeighbors :: [NumInfo] -> Int
prodNeighbors ns = foldr ((*) . valN) 1 ns

sl2 = parseForSymbol [] (tl2, [0..]) 1
dl1 = parseForAny ParseNumInfo { curNum = emptyNum, numList = [] } (tl1, [0..]) 0
dl2 = parseForAny ParseNumInfo { curNum = emptyNum, numList = [] } (tl2, [0..]) 1

