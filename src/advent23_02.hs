module Main where

import Data.Char ( digitToInt, isDigit )

testLine = "Game 1: 4 green, 2 blue; 1 red, 1 blue, 4 green; 3 green, 4 blue, 1 red; 7 green, 2 blue, 4 red; 3 red, 7 green; 3 red, 3 green"

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 02 - Part 1                                              **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input02.txt"
  -- content <- readFile "Input23/test02_1.txt"
  let processed = map (process Parser { val = emptyGame
                                      , toParse = []}) (lines content)
  mapM_ (putStrLn . show) processed
  let validID = foldr (toBeAdded . val) [] processed
  mapM_ (putStrLn . show) validID
  let sumValid = sum validID
  -- let res = sum (map calibrationValue (lines content))
  putStrLn $ "Answer 1> " ++ show sumValid
  let processedAdv = map (processAdv Parser { val = emptyGame
                                            , toParse = []}) (lines content)
  mapM_ (putStrLn . show) processedAdv
  let powersList = map (powerGame . val) processedAdv
  mapM_ (putStrLn . show) powersList
  -- let resFull = sum (map calibrationValueFull (lines content))
  putStrLn $ "Answer 2> " ++ show (foldr (+) 0 powersList)
  putStrLn "END"

data Parser a = Parser { val :: a
                       , toParse :: String }
                deriving Show

getID :: Parser Int -> String -> Parser Int
getID pInfo []  = pInfo { toParse=[] }
getID pInfo (c:cs)
  | isDigit c = getID Parser { val = 10 * val pInfo + digitToInt c
                             , toParse = cs } cs
  | elem c [':',' '] = pInfo { toParse = cs}
  -- | c == ' ' = getID cs intData { toParseInt = cs }
  | otherwise = Parser { val=0, toParse=(c:cs) }


data Game = Game { idgame :: Int
                 , red :: Int
                 , green :: Int
                 , blue :: Int
                 } deriving Show

emptyGame = Game { idgame = -1, red = 0, green = 0, blue = 0 }
targetGame = Game { idgame = -2, red=12, green=13, blue=14 }

readBall :: Parser Game -> String -> Parser Int -> Parser Game
readBall gameData ('g':'r':'e':'e':'n':cs) intData = gameData { val = updatedGame
                                                              , toParse = cs }
  where nbGreen = green (val gameData) + val intData
        updatedGame = (val gameData) { green = nbGreen }
readBall gameData ('b':'l':'u':'e':cs) intData = gameData { val = updatedGame
                                                          , toParse = cs }
  where nbBlue = blue (val gameData) + val intData
        updatedGame = (val gameData) { blue = nbBlue }
readBall gameData ('r':'e':'d':cs) intData = gameData { val = updatedGame
                                                      , toParse = cs }
  where nbRed = red (val gameData) + val intData
        updatedGame = (val gameData) { red = nbRed }
readBall gameData line _ = readBall gameData stillToParse parsedInt
  where parsedInt = getID Parser { val=0, toParse=line } line
        stillToParse = toParse parsedInt

process :: Parser Game -> String -> Parser Game
process gameData [] = gameData { toParse=[] }
process gameData ('G':'a':'m':'e':' ':cs) = process gameData{ val=gameWithID
                                                            , toParse=stillToParse} stillToParse
  where parsedID = getID Parser { val=0, toParse=cs } cs
        gameWithID = (val gameData) { idgame = val parsedID }
        stillToParse = toParse parsedID

-- ' ' before nb ball, as ' ' after nb ball is consumed by getID
process gameData (' ':cs) = process gameData {toParse=cs} cs
process gameData (',':cs) = process gameData {toParse=cs} cs
process gameData (';':cs) = process gameData {val=gameReset, toParse=cs} cs
  where gameReset = emptyGame {idgame = (idgame (val gameData))}

-- early stopping if not valid
process gameData line
  | validGame targetGame (val gameWithNewBall) = process gameWithNewBall stillToParse
  | otherwise = gameWithNewBall
  where gameWithNewBall = readBall gameData {toParse=line}
                                   line
                                   Parser { val=0, toParse=line}
        stillToParse = toParse gameWithNewBall

-- ':' after Game ID should be consumed by readBall
-- process gameData line = process gameWithNewBall stillToParse
--   where gameWithNewBall = readBall gameData {toParse=line}
--                                    line
--                                    Parser { val=0, toParse=line}
--         stillToParse = toParse gameWithNewBall
-- redundant
-- process gameData line = gameData { toParse="line: "++line }

validGame :: Game -> Game -> Bool
validGame gameMax game = blue game <= blue gameMax &&
                         green game <= green gameMax &&
                         red game <= red gameMax

toBeAdded :: Game -> [Int] -> [Int]
toBeAdded game currentList
  | validGame targetGame game = (idgame game):currentList
  | otherwise = currentList



readBallAdv :: Parser Game -> String -> Parser Int -> Parser Game
readBallAdv gameData ('g':'r':'e':'e':'n':cs) intData = gameData { val = updatedGame
                                                              , toParse = cs }
  where nbGreen = max (green (val gameData)) (val intData)
        updatedGame = (val gameData) { green = nbGreen }
readBallAdv gameData ('b':'l':'u':'e':cs) intData = gameData { val = updatedGame
                                                          , toParse = cs }
  where nbBlue = max (blue (val gameData)) (val intData)
        updatedGame = (val gameData) { blue = nbBlue }
readBallAdv gameData ('r':'e':'d':cs) intData = gameData { val = updatedGame
                                                      , toParse = cs }
  where nbRed = max (red (val gameData)) (val intData)
        updatedGame = (val gameData) { red = nbRed }
readBallAdv gameData line _ = readBallAdv gameData stillToParse parsedInt
  where parsedInt = getID Parser { val=0, toParse=line } line
        stillToParse = toParse parsedInt

processAdv :: Parser Game -> String -> Parser Game
processAdv gameData [] = gameData { toParse=[] }
processAdv gameData ('G':'a':'m':'e':' ':cs) = processAdv gameData{ val=gameWithID
                                                            , toParse=stillToParse} stillToParse
  where parsedID = getID Parser { val=0, toParse=cs } cs
        gameWithID = (val gameData) { idgame = val parsedID }
        stillToParse = toParse parsedID

-- ' ' before nb ball, as ' ' after nb ball is consumed by getID
processAdv gameData (' ':cs) = processAdv gameData {toParse=cs} cs
processAdv gameData (',':cs) = processAdv gameData {toParse=cs} cs
processAdv gameData (';':cs) = processAdv gameData {toParse=cs} cs

-- ':' after Game ID should be consumed by readBall
processAdv gameData line = processAdv gameWithNewBall stillToParse
  where gameWithNewBall = readBallAdv gameData {toParse=line}
                                   line
                                   Parser { val=0, toParse=line}
        stillToParse = toParse gameWithNewBall
-- redundant
-- processAdv gameData line = gameData { toParse="line: "++line }

powerGame :: Game -> Int
powerGame game = blue game * red game * green game
