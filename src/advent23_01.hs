module Main where

import Data.Char ( digitToInt, isDigit )


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 01 - Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input01.txt"
  let res = sum (map calibrationValue (lines content))
  putStrLn $ "Answer 1> " ++ show res
  let resFull = sum (map calibrationValueFull (lines content))
  putStrLn $ "Answer 2>" ++ show resFull

calibrationValue :: String -> Int
calibrationValue line = 10 * findDigit line + findDigit (reverse line)

findDigit :: String -> Int
findDigit [] = 0   -- should be error
findDigit (c:cs)
  | Data.Char.isDigit c = Data.Char.digitToInt c
  | otherwise = findDigit cs

calibrationValueFull :: String -> Int
calibrationValueFull line = 10 * findDigitFull line + lastDigitFull (reverse line)

findDigitFull :: String -> Int
findDigitFull [] = 0   -- should be error
findDigitFull ('o':'n':'e':_) = 1
findDigitFull ('t':'w':'o':_) = 2
findDigitFull ('t':'h':'r':'e':'e':_) = 3
findDigitFull ('f':'o':'u':'r':_) = 4
findDigitFull ('f':'i':'v':'e':_) = 5
findDigitFull ('s':'i':'x':_) = 6
findDigitFull ('s':'e':'v':'e':'n':_) = 7
findDigitFull ('e':'i':'g':'h':'t':_) = 8
findDigitFull ('n':'i':'n':'e':_) = 9
findDigitFull (c:cs)
  | isDigit c = digitToInt c
  | otherwise = findDigitFull cs

lastDigitFull :: String -> Int
lastDigitFull [] = 0
lastDigitFull ('e':'n':'o':_) = 1
lastDigitFull ('o':'w':'t':_) = 2
lastDigitFull ('e':'e':'r':'h':'t':_) = 3
lastDigitFull ('r':'u':'o':'f':_) = 4
lastDigitFull ('e':'v':'i':'f':_) = 5
lastDigitFull ('x':'i':'s':_) = 6
lastDigitFull ('n':'e':'v':'e':'s':_) = 7
lastDigitFull ('t':'h':'g':'i':'e':_) = 8
lastDigitFull ('e':'n':'i':'n':_) = 9
lastDigitFull (x:xs)
  | isDigit x = digitToInt x
  | otherwise = lastDigitFull xs
