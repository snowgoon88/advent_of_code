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

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
import Data.Char ( digitToInt )
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.Foldable ( foldl' )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List.Extra ( splitOn, sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import qualified Numeric.LinearAlgebra as NL
import qualified Numeric.LinearAlgebra.Data as NLD

import System.Environment (getArgs)

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
  putStrLn "** Advent 2025 - Day 10 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let ms = map parseMachine (lines content)
  putStrLn $ "ms=" ++ show ms

  let mach00 = breadthPress (ms !! 0) (0, 10) [offLight (ms !! 0)]
  putStrLn $ "mach00=" ++ show mach00
  let res = map (\m -> breadthPress m (0,10) [offLight m]) ms
  putStrLn $ "res=" ++ show res
  let pRes = sum res
  putStrLn $ "Answer 1> " ++ show pRes

  -- let jolt00 = breadthJolt (ms !! 0) (0, 500) [zeroJolt (ms !! 0)]
  -- putStrLn $ "jolt00=" ++ show jolt00
  -- let matA = btnToMat (ms !! 0)
  -- let vJ = joltToVect (ms !! 0)
  -- putStrLn $ "LS=" ++ show vJ ++ "\n" ++ show matA
  -- let j00 = solveJolt (ms !! 0)
  -- putStrLn $ "j00=" ++ show j00
  -- let pot00 = intSol (snd j00)
  -- putStrLn $ "pot00=" ++ show pot00
  -- let aBtn00 = btnToVec (ms !! 0)
  -- putStrLn $ "aBtn00=" ++ show aBtn00
  -- let sols00 = map (computeJoltage (ms !! 0) aBtn00) pot00
  -- putStrLn $ "sols00=" ++ show sols00
  -- let iSol00 = findIntegerSol (ms !! 0) (snd j00)
  -- putStrLn $ "iSol00=" ++ show iSol00

  -- let iSols = map findIntegerSol ms
  -- putStrLn $ "iSols=" ++ show iSols
  -- mapM_ (\m -> do
  --           print "*******************************"
  --           putStrLn $ "Mach=m" ++ show m
  --           let iSol = findIntegerSol m
  --           putStrLn $ "iSol=" ++ show iSol )
  --   ms

  -- let resJ = map solveJolt ms
  -- -- let resJ = map (\m -> breadthJolt m (0,1000) [zeroJolt m]) ms
  -- putStrLn $ "resJ=" ++ show resJ
  -- let iRes = map (floor . fst) resJ
  -- putStrLn $ "resJ=" ++ show iRes
  -- let cRes = sum iRes

  mapM_ (\m -> do
            print "*******************************"
            putStrLn $ "Mach=m" ++ show m
            let bEffect = btnToVec m
            -- putStrLn $ "bEffect=" ++ show bEffect
            let reaSol = solveJolt m
            -- putStrLn $ "real=" ++ show reaSol
            let iSol = map round (NLD.toList (snd reaSol))
            let iSum = sum iSol
            let tentativeSol = joltage bEffect iSol
            putStrLn $ "goal______" ++ show (m_jol m)
            putStrLn $ "tentative=" ++ show tentativeSol ++ " =>min=" ++ show iSum
            let iSol = findIntegerSol m
            let toPrint = case iSol of
                  (Nothing, preal) -> ("NO SOL, pReal=" ++ show preal)
                  (Just m, _)      -> show m
            print toPrint
            -- putStrLn $ "intSol=" ++ show (findIntegerSol m)
            -- let allSols = intSolutions bEffect (m_jol m) [[]] []
            -- putStrLn $ "allSols=" ++ show allSols
            -- let best = bestSol allSols
            -- putStrLn $ "best=" ++ show best
        )
    ms
  let rSols = map solveJolt ms
  let iSols = map (\s -> map round (NLD.toList (snd s))) rSols
  let iSum = map sum iSols
  putStrLn $ "iSum =" ++ show iSum
  let cRes = sum iSum
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Light = String
type Button = String
data Machine = M { m_lig :: Light
                 , m_btn :: [Button]
                 , m_jol :: [Int] }
  deriving ( Show )

removeSurronder :: [a] -> [a]
removeSurronder str = reverse (drop 1 ( reverse (drop 1 ( str ))))

makeBtnPattern :: Int -> [Int] -> Button
makeBtnPattern len bts = map setup [0 .. len-1]
  where
    setup i = if elem i bts then '1' else '0'

parseMachine :: String -> Machine
parseMachine line = M lightGoal buttons joltage
  where
    tok = words line
    lightGoal = removeSurronder ( tok !! 0 )
    joltage = map read (splitOn "," (removeSurronder (last tok)))
    decodeBtn str = makeBtnPattern (length lightGoal) $ map read (splitOn "," (removeSurronder str))
    buttons = map decodeBtn (removeSurronder tok)

offLight :: Machine -> Light
offLight mach = replicate (length (m_lig mach)) '.'

switchLight :: (Char, Char) -> Char
switchLight ('0', c) = c
switchLight ('1', '#') = '.'
switchLight ('1', '.') = '#'
switchLight p = error ( "switch light with " ++ show p )

pushBtn :: Light -> Button -> Light
pushBtn light btn = zipWith (curry switchLight) btn light

breadthPress mach (d, maxDepth) statusL
  | d == maxDepth = -maxDepth
  | elem (m_lig mach) newStatus = d+1
  | otherwise = breadthPress mach (d+1, maxDepth) (Set.toList (Set.fromList newStatus))
  where
    newStatus = [pushBtn l b | l <- statusL, b <- m_btn mach]

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

zeroJolt :: Machine -> [Int]
zeroJolt mach = replicate (length (m_jol mach)) 0

incJolt :: (Char, Int) -> Int
incJolt ('0', i) = i
incJolt ('1', i) = i+1
incJolt p = error ( "incJolt with " ++ show p )

pushJolt :: [Int] -> Button -> [Int]
pushJolt jolt btn = zipWith (curry incJolt) btn jolt

breadthJolt mach (d, maxDepth) statusJ
  | d == maxDepth = -maxDepth
  | elem (m_jol mach) newStatus = d+1
  | otherwise = breadthJolt mach (d+1, maxDepth) (Set.toList (Set.fromList filtered))
  where
    newStatus = [pushJolt l b | l <- statusJ, b <- m_btn mach]
    filtered = filter filterOut newStatus
    filterOut stat = all (>=0) $ zipWith (-) (m_jol mach) stat

-- SMART ??? use Linear Algebra ?? => Double
type ActMat = NLD.Matrix Double
type JoltVec = NLD.Vector Double
btnToMat :: Machine -> ActMat
btnToMat mach = NLD.tr $ (NLD.><) nbAct nbGoal dataMat
  where
    nbGoal = length (m_jol mach)
    nbAct = length (m_btn mach)
    dataMat = map (fromIntegral . digitToInt) $ concat (m_btn mach)

joltToVect :: Machine -> JoltVec
joltToVect mach = NL.vector (map fromIntegral (m_jol mach))

solveJolt :: Machine -> (Double, JoltVec)
solveJolt mach = (sum (NLD.toList sol), sol)
  where
    matAction = btnToMat mach
    vecJolt = joltToVect mach
    sol = matAction NL.<\> vecJolt


-- from listOfDouble solution to listOfInt
intSol :: NLD.Vector Double -> [[Int]]
intSol solD = foldr opSol [[]] lowLimit
  where
    lowLimit = map floor (NLD.toList solD)
    opSol nb sol = [i:s | i <- [nb-1 .. nb+1], s <- sol ]

-- findIntegerSol mach = (validSolsSumPress, realSol)
findIntegerSol mach
  | null validSolsSumPress = (Nothing, NLD.toList realSol)
  | otherwise = (Just (take 1 (sortOn (snd . fst) validSolsSumPress)), [])
  where
    realSol = snd $ solveJolt mach
    aVec = btnToVec mach
    pSol = intSol realSol
    potSols :: [( ([Int], Int), [Int]) ]
    potSols = map (\s -> (computeJoltage mach aVec s, s)) pSol
    validSolsSumPress = filter ((==m_jol mach) . fst . fst) potSols

computeJoltage :: Machine -> [[Int]] -> [Int] -> ([Int], Int)
computeJoltage mach actionVec iSol = allSols
  where
    initJolt = zeroJolt mach
    allSols = (foldr opBtn initJolt (zip iSol actionVec), sum iSol)
    -- multiply the effect of a button by nbPress and add to acc
    opBtn :: (Int, [Int]) -> [Int] -> [Int]
    opBtn (nbPress, btnEffect) acc = zipWith (+) acc
                                                 (map (*nbPress) btnEffect)

btnToVec :: Machine -> [[Int]]
btnToVec mach = map toVec (m_btn mach)
  where
    toVec = map digitToInt

-- Constraint Solving, One action by one action
exBtn0 = map (map digitToInt) ["0001","0101","0010","0011","1010","1100"]
exJolt0 = [3,5,4,7] :: [Int]
exBtn1 = map (map digitToInt) ["10111","00110","10001","11100","01111"]
exJolt = [7,5,12,7,2] :: [Int]

exBtnF01 = map (map digitToInt) ["00111101","11001001","10111000","10010110","11000110","10011111","00001010","00100001","10110111","10101101"]
exGoltF01 = [84,24,46,50,59,84,57,64]

-- maxTime a button can be Pushed, givent jolt_goal and btnEffect
maxVal vecBtnEffect prevNbPush goalJolt = (minVal, (maxCoef, updatedTarget, prevEffect))
  where
    initJolt = replicate (length goalJolt) 0
    prevEffect = foldr opBtn initJolt (zip prevNbPush vecBtnEffect)
    updatedTarget = zipWith (-) goalJolt prevEffect
    maxCoef = zipWith (divMaybe) updatedTarget (vecBtnEffect !! length prevNbPush)
    minVal = minMaybe 1000 maxCoef
    -- multiply the effect of a button by nbPress and add to acc
    opBtn :: (Int, [Int]) -> [Int] -> [Int]
    opBtn (nbPress, btnEffect) acc = zipWith (+) acc
                                                 (map (*nbPress) btnEffect)
    -- division or Nothing
    divMaybe x d = if d == 0 then Nothing else Just (div x d)
    -- minMaybe
    minMaybe acc [] = acc
    minMaybe acc (Nothing:ms) = minMaybe acc ms
    minMaybe acc (Just v:ms) = minMaybe (min acc v) ms

joltage :: [[Int]] -> [Int] -> [Int]
joltage vecBtnEffect pushedBtn = foldr opBtn initJolt (zip pushedBtn vecBtnEffect)
  where
    initJolt = replicate (length (head vecBtnEffect)) 0
    -- multiply the effect of a button by nbPress and add to acc
    opBtn :: (Int, [Int]) -> [Int] -> [Int]
    opBtn (nbPress, btnEffect) acc = zipWith (+) acc
                                                 (map (*nbPress) btnEffect)

intSolutions :: [[Int]] -> [Int] -> [[Int]] -> [[Int]] -> [[Int]]
intSolutions _ _ [] acc = acc
intSolutions vecBtnEffect goalJolt (csol:curSolList) acc
  | length csol == length vecBtnEffect = if goalJolt == (joltage vecBtnEffect csol)
    then intSolutions vecBtnEffect goalJolt curSolList (csol:acc)
    else intSolutions vecBtnEffect goalJolt curSolList acc
  | otherwise = intSolutions vecBtnEffect goalJolt (addedSol ++ curSolList) acc
    where
      coefRange = maxVal vecBtnEffect csol goalJolt
      addedSol = [csol ++ [i] | i <- [0 .. fst coefRange]]

bestSol sols = minimum (map sum sols)
