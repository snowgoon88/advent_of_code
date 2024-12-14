module Main where

-- import Data.List ( group, sort, sortOn, sortBy )
import Data.String.Utils ( replace, split )
import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 08 - Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input08.txt"
  -- content <- readFile "Input23/test08_1.txt"
  -- content <- readFile "Input23/test08_2.txt"

  let path = head ( lines content )
  -- print $ "path=" ++ show path
  let readNodes = map parseNode (drop 2 (lines content))
  -- print readNodes
  let mapNode = Map.fromList readNodes
  -- print mapNode
  -- let n1 = nextNode mapNode "AAA" 'L'
  -- print n1
  -- let lenPath = applyPath mapNode (0, "AAA") path
  -- print lenPath
  let terminalPath = applyPathUntil mapNode isEndLabel (0, "AAA") (cycle path)
  -- -- print $ "best_path=" ++ show terminalPath

  let pRes = fst terminalPath
  putStrLn $ "Answer 1> " ++ show pRes

  let allKeys = Map.keys mapNode
  let allStart = filter isStartLabel allKeys
  print $ "startKeys=" ++ show allStart

  -- BRUTE FORCE NOT WORKING
  -- let res = applyPathsUntil mapNode isEndLabel (0, allStart) (cycle path)
  -- print $ "res=" ++ show res

  -- MAIS on s'apercoit que chaque path est en fait répétitif et indépendant
  -- let loop01 = detectLoops mapNode 10 isEndLabel (0, head allStart) (cycle path) []
  -- print $ "loop01" ++ show loop01
  -- let period01 = computePeriod loop01
  -- print $ "period01" ++ show period01
  -- mapM_ (print . computePeriod . (\label -> detectLoops mapNode 2 isEndLabel (0, label) (cycle path) []))
  --   allStart
  -- donc on peut faire une liste de ces périodes et chercher un élément commun

  -- all the periods
  let allPeriodsLabel = map (computePeriod . (\label -> detectLoops mapNode 1 isEndLabel (0, label) (cycle path) [])) allStart
  -- print $ "allPeriodsLabel=" ++ show allPeriodsLabel
  let allPeriods = map (fst . head) allPeriodsLabel
  -- print $ "allPeriods=" ++ show allPeriods
  let factors = stepDivide lazyPrimes allPeriods []
  -- print $ "factors=" ++ show factors
  let cRes = product factors
  putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

data Node = Node { label :: String
                 , left :: String
                 , right :: String }
  deriving Show

type Path = String
type PathLong = (Int, String)
type MapNode = Map.Map String Node

parseNode :: String -> (String, Node)
parseNode line = (labelR, Node { label=labelR, left=leftR, right=rightR })
  where
    noSpace = replace ")" "" (replace "(" "" ( replace " " "" line))
    splitLabel = split "=" noSpace
    labelR = head splitLabel
    splitBranch = split "," (head (tail splitLabel))
    leftR = head splitBranch
    rightR = head (tail splitBranch)


applyPathUntil :: MapNode -> (String -> Bool) -> PathLong -> Path -> PathLong
applyPathUntil mapNodes condF (nbStep, pos) (p:path)
  | okStop    = (nbStep, pos)
  | otherwise = applyPathUntil mapNodes condF (nbStep+1, nextPos) path
    where okStop = condF pos
          nextPos = nextNode mapNodes pos p

isEndLabel :: String -> Bool
isEndLabel pos = (last pos) == 'Z'

applyPath _ (nbStep,pos) [] = (nbStep,pos)
applyPath mapNodes (nbStep,pos) (p:path) = applyPath mapNodes (nbStep+1, nextPos) path
  where nextPos = nextNode mapNodes pos p

nextNode :: MapNode -> String -> Char -> String
nextNode mapNodes pos 'R' =
  case Map.lookup pos mapNodes of
    Just n -> (right n)
    Nothing -> error ("Node " ++ pos ++ " not found")
nextNode mapNodes pos 'L' =
  case Map.lookup pos mapNodes of
    Just n -> (left n)
    Nothing -> error ("Node " ++ pos ++ " not found")

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

isStartLabel :: String -> Bool
isStartLabel pos = (last pos) == 'A'

type ParallelPath = (Int, [String])

-- try to detect loops
-- first nbX event of isEndLabel

detectLoops :: MapNode -> Int -> (String -> Bool) -> PathLong -> Path -> [PathLong] -> [PathLong]
detectLoops mapNodes nbX condF (nbStep, pos) (p:path) resSoFar
  | length resSoFar == nbX = resSoFar
  | onEndLabel = detectLoops mapNodes nbX condF (nbStep+1, nextPos) path ((nbStep, pos):resSoFar)
  | otherwise = detectLoops mapNodes nbX condF (nbStep+1, nextPos) path resSoFar
    where onEndLabel = condF pos
          nextPos = nextNode mapNodes pos p

computePeriod :: [PathLong] -> [PathLong]
computePeriod ((f1, pos1):(f2, pos2):ps) = (f1-f2, pos1): computePeriod ((f2, pos2):ps)
computePeriod [(f,pos)] = [(f, pos)]

-- lazt compute prime numbers
{-
The main idea is that the candidates for the next primes already contain no numbers
that are divisible by any prime less than the first prime given to the function.
So that if you call

   calcNextPrimes (5:ps) [11,13,17..]

the candidate list contains no number, that is divisible by 2 or 3, that means that
the first non-prime candidate will be 5 * 5, cause 5* 2 and 5 * 3 and 5 * 4 are
already eliminated. That allows you to take all candidates, that are smaller than
the square of 5 and add them straight away to the primes and sieve the rest to
eliminate all numbers divisible by 5.
-}
--lazyPrimes :: [Integer]
lazyPrimes :: [Int]
lazyPrimes = 2: 3: calcNextPrimes (tail lazyPrimes) [5, 7 .. ]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates in
      smallerSquareP ++ calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]

-- To find the PPCM des nombres, on décompose en facteur premier.
-- see https://www.alloprof.qc.ca/fr/eleves/bv/mathematiques/plus-petit-commun-multiple-ppcm-m1063
divIfPossible :: Int -> Int -> Int
divIfPossible prime x
  | canDivide prime x = div x prime
  | otherwise =  x

canDivide :: Int -> Int -> Bool
canDivide prime x = mod x prime == 0

stepDivide [] listNum soFar = soFar
stepDivide (p:primes) listNum soFar
  | product listNum == 1 =  soFar
  | any (canDivide p) listNum = stepDivide (p:primes) (map (divIfPossible p) listNum) (p:soFar)
  | otherwise = stepDivide primes listNum soFar

-- same than stepDivide but keep steps
stepDivideDebug [] listNum soFar = soFar
stepDivideDebug (p:primes) listNum soFar
  | product listNum == 1 =  soFar
  | any (canDivide p) listNum = stepDivideDebug (p:primes) newListNum ((p, newListNum):soFar)
  | otherwise = stepDivideDebug primes listNum soFar
    where newListNum = map (divIfPossible p) listNum



-- BRUTE FORCE IS NOT WORKING
applyPathsUntil :: MapNode -> (String -> Bool) -> ParallelPath -> Path -> ParallelPath
applyPathsUntil mapNodes condF (nbStep, posList) (p:path)
  | okStop    = (nbStep, posList)
  | otherwise = applyPathsUntil mapNodes condF (nbStep+1, nextPosList) path
    where okStop = all condF posList
          nextPosList = map (\po -> nextNode mapNodes po p) posList
