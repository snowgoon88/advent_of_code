module Main where

-- import qualified MyParser as MP
-- import Data.List ( group, sort, sortOn, sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( foldM )
import qualified Data.Map as Map
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.Char ( ord )

l1 = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 15 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input15.txt"
  -- content <- readFile "Input23/test15_1.txt"

  let sequences = split "," content
  -- print $ "sequences=" ++ show sequences
  let hashList = map (foldl opHash 0) sequences
  -- print $ "hashList=" ++ show hashList
  let pRes = sum hashList
  putStrLn $ "Answer 1> " ++ show pRes

  let boxMap = Map.fromList (zip [0..255] (repeat []))
  let res = foldl decodeStep boxMap sequences
  -- print $ "res=" ++ show res

  let cRes = Map.foldlWithKey powerSum 0 res
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

opHash :: Int -> Char -> Int
opHash hash '\n' = hash
opHash hash c = (hash + ord c) * 17 `rem` 256

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

type Lens = (String, Int)
type MapBox = Map.Map Int [Lens]

removeLens :: Int -> String -> MapBox -> MapBox
-- removeLens box label arg = "Remove " ++ label ++ " from box " ++ show box
removeLens box label boxmap = Map.adjust (removeLensBox label) box boxmap
removeLensBox :: String -> [Lens] -> [Lens]
removeLensBox label [] = []
removeLensBox label ((l_label, l_foc):ls)
  | label == l_label = ls
  | otherwise = (l_label, l_foc): removeLensBox label ls

addLens :: Int -> Lens -> MapBox -> MapBox
-- addLens :: Int -> Lens -> String -> String
-- addLens box (label, focal) arg = "Add lense lab=" ++ label ++ ", foc=" ++ show focal ++ " from box " ++ show box
addLens box lens boxmap = Map.adjust (addLensBox lens) box boxmap
addLensBox :: Lens -> [Lens] -> [Lens]
addLensBox lens [] = [lens]
addLensBox (label, focal) ((l_label, l_focal):ls)
  | label == l_label = (l_label, focal):ls
  | otherwise = (l_label, l_focal) : addLensBox (label, focal) ls

decodeStep :: MapBox -> String -> MapBox
decodeStep mapbox step
  | last step == '-' = removeLens (foldl opHash 0 labelMinus) labelMinus mapbox
  | otherwise = addLens (foldl opHash 0 labelEqual) (labelEqual, focalEqual) mapbox
    where
      labelMinus = head (split "-" step)
      splitEqual = split "=" step
      labelEqual = head splitEqual
      focalEqual = read $ head (drop 1 splitEqual) :: Int

powerSum :: Int -> Int -> [Lens] -> Int
powerSum curSum box lenses = curSum + (box+1) * sum (map (\(slot, (_, focal)) -> slot * focal) (zip [1..] lenses))
