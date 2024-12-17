{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
-- import Data.List ( find, sortOn, groupBy ) -- sortOn, groupBy, find, group, sort, sortBy _)
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( split, join) --, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
-- import Control.Monad ( fold )
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( delete, sortOn ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace

co01 = CO (729, 0, 0) [] 0
mem01 = [0, 1, 5, 4, 3, 0]

co02 = CO (53437164, 0, 0) [] 0
mem02 = [2,4,1,7,7,5,4,1,1,4,5,5,0,3,3,0]

co03 = CO (117440, 0, 0) [] 0
mem03 = [0, 3, 5, 4, 3, 0]

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 17 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  -- content <- readFile "Input24/input17.txt"
  -- content <- readFile "Input24/test17_1.txt"

  -- let co01 = CO (729, 0, 0) [] 0
  -- let mem01 = [0, 1, 5, 4, 3, 0]
  let comp = co02
  let memory = mem02

  -- let debugRun nb = do
  --       let p01 = runProgram (nb, 0) (co01, mem01)
  --       print $ "== step " ++ show p01 ++ " ============="
  --       print $ "p01=" ++ show p01

  -- mapM_ debugRun [0..10]

  let run01 = runProgram (-1, 0) (comp, memory)
  print $ "run01=" ++ show run01


  let run03 = runProgram (-1, 0) (co03, mem03)
  print $ "run03=" ++ show run03
  -- let debugRun nb = do
  --       let p01 = runProgram (nb, 0) (co01, mem01)
  --       print $ "== step " ++ show p01 ++ " ============="
  --       print $ "p01=" ++ show p01

  -- mapM_ debugRun [0..10]

  let runMult nb = do
        let pRun = runProgram (-1, 0) (CO (nb, 0, 0) [] 0, mem02)
        print $ "== A=" ++ show nb
        print $ "pRun=" ++ show pRun

  -- mapM_ runMult [0..64]
  -- mapM_ runMult ([3, 3*8 + 0, (3*8 + 0) * 8 + 7, ((3*8+0)*8+7)*8 + 4, (((3*8+0)*8+7)*8 + 4)*8, (((3*8+0)*8+7)*8 + 4)*8 + 1, ((((3*8+0)*8+7)*8 + 4)*8 + 1) * 8
  --               , (((((3*8+0)*8+7)*8 + 4)*8 + 1)*8)*8+3
  --               , ((((((3*8+0)*8+7)*8 + 4)*8 + 1)*8)*8+3)*8+2
  --               , (((((((3*8+0)*8+7)*8 + 4)*8 + 1)*8)*8+3)*8+2)*8+1
  --               , ((((((((3*8+0)*8+7)*8 + 4)*8 + 1)*8)*8+3)*8+2)*8+1)*8+2
  --               , (((((((((3*8+0)*8+7)*8 + 4)*8 + 1)*8)*8+3)*8+2)*8+1)*8+2)*8 ]
  --               ++ [ (((((((((3*8+0)*8+7)*8 + 4)*8 + 1)*8)*8+3)*8+2)*8+1)*8+d)*8 + c | c <- [0..8], d <- [0..16]])

  -- print "--------------------------"
  -- mapM_ runMult ([3, 3*8+0, 3*8+7
  --               , (((((((((3*8+7)*8+7)*8 + 4)*8 + 1)*8)*8+3)*8+2)*8+1)*8+2)*8 ]
  --               ++ [(((((((((3*8+7)*8+7)*8 + 4)*8 + 1)*8)*8+3)*8+2)*8+1)*8+2)*8+c | c <- [0..8]])

  -- print "--------------------------"
  -- mapM_ runMult ([3, 3*8+0, 3*8+7
  --               , (((((((((3*8+7)*8+7)*8 + 4)*8 + 1)*8)*8+3)*8+2)*8+2)*8+2)*8 ]
  --               ++ [(((((((((3*8+7)*8+7)*8 + 4)*8 + 1)*8)*8+3)*8+2)*8+2)*8+2)*8+c | c <- [0..8]])

  let pRes = cOut (fst (snd run01))
  putStrLn $ "Answer 1> " ++ show pRes

  let step01 = opLook mem02 0
  print $ "step01=" ++ show step01

  let step02 = opLookNext mem02 step01
  print $ "step02=" ++ show step02

  let step03 = opLookNext mem02 step02
  print $ "step03=" ++ show step03

  let step04 = opLookNext mem02 step03
  print $ "step04=" ++ show step04

  let step05 = opLookNext mem02 step04
  print $ "step05=" ++ show step05

  let step06 = opLookNext mem02 step05
  print $ "step06=" ++ show step06

  let step07 = opLookNext mem02 step06
  print $ "step07=" ++ show step07

  let step08 = opLookNext mem02 step07
  print $ "step08=" ++ show step08

  let step09 = opLookNext mem02 step08
  print $ "step09=" ++ show step09

  let step10 = opLookNext mem02 step09
  print $ "step10=" ++ show step10

  let step11 = opLookNext mem02 step10
  print $ "step11=" ++ show step11

  let step12 = opLookNext mem02 step11
  print $ "step12=" ++ show step12

  let step13 = opLookNext mem02 step12
  print $ "step13=" ++ show step13

  let step14 = opLookNext mem02 step13
  print $ "step14=" ++ show step14

  let step15 = opLookNext mem02 step14
  print $ "step15=" ++ show step15

  let step16 = opLookNext mem02 step15
  print $ "step16=" ++ show step16

  let cRes = minimum (map fst step16)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Computer = CO { cReg :: (Int, Int, Int)
                   , cOut :: [Int]
                   , cPt :: Int } deriving Show
type Memory = [Int]


combo :: Computer -> Int -> Int
combo (CO (rA, rB, rC) out pt) 4 = rA
combo (CO (rA, rB, rC) out pt) 5 = rB
combo (CO (rA, rB, rC) out pt) 6 = rC
combo comp 7 = error ("combo 7")
combo _ val = val

applyOp :: Computer -> Memory -> (Int, Int) -> (Computer, Memory)
-- adv
applyOp (CO (rA, rB, rC) out pt) mem (0, val) = (CO (newA, rB, rC) out (pt+2), mem)
  where newA = div rA $ 2 ^ (combo (CO (rA, rB, rC) out pt) val)
-- bxl
applyOp (CO (rA, rB, rC) out pt) mem (1, val) = (CO (rA, newB, rC) out (pt+2), mem)
  where newB = Bits.xor rB val
-- bst
applyOp (CO (rA, rB, rC) out pt) mem (2, val) = (CO (rA, newB, rC) out (pt+2), mem)
  where newB = mod (combo (CO (rA, rB, rC) out pt) val) 8
-- jnz
applyOp (CO (rA, rB, rC) out pt) mem (3, val)
  | rA == 0 = (CO (rA, rB, rC) out (pt+2), mem)
  | otherwise = (CO (rA, rB, rC) out val, mem)
-- bxc
applyOp (CO (rA, rB, rC) out pt) mem (4, val) = (CO (rA, newB, rC) out (pt+2), mem)
  where newB = Bits.xor rB rC
-- out
applyOp (CO (rA, rB, rC) out pt) mem (5, val) = (CO (rA, rB, rC) newOut (pt+2), mem)
  where newOut = out ++ [mod (combo (CO (rA, rB, rC) out pt) val) 8]
-- bdv
applyOp (CO (rA, rB, rC) out pt) mem (6, val) = (CO (rA, newB, rC) out (pt+2), mem)
  where newB = div rA $ 2 ^ (combo (CO (rA, rB, rC) out pt) val)
-- cdv
applyOp (CO (rA, rB, rC) out pt) mem (7, val) = (CO (rA, rB, newC) out (pt+2), mem)
  where newC = div rA $ 2 ^ (combo (CO (rA, rB, rC) out pt) val)

runProgram :: (Int, Int) -> (Computer, Memory) -> (Int, (Computer, Memory))
runProgram (maxIt, curIt) (comp, mem)
  | maxIt < 0 = nextstep
  | curIt >= maxIt = (curIt, (comp, mem))
  | otherwise = nextstep
  where
    pt = (cPt comp)
    nextstep = if pt < (length mem) - 1
                   then runProgram (maxIt, curIt+1) (applyOp comp mem (mem !! pt, mem !! (pt+1))) -- `debug` ("op,val = " ++ show (mem !! pt) ++ ", " ++ show (mem !! (pt+1))
                   else (-curIt, (comp, mem))



-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

opLook :: [Int] -> Int -> [(Int, [Int])]
opLook goal regA = filter (\c -> snd c == partialGoal) res
  where
    res = map (\a -> (a, cOut $ fst (snd (runProgram (-1, 0) (CO (regA+a, 0, 0) [] 0, mem02)))))
                       [0..7]
    resLen = length (snd ( head res ))
    partialGoal = drop (length goal - resLen) goal

--opLookNext :: [Int] -> [(Int, [Int]) -> [(Int, [Int])]
opLookNext goal regAList = concat $ map (\r -> combine (fst r * 8)) regAList
  where
    previous val = opLook goal val
    combine val = map (\prev -> (val + fst prev, snd prev)) (previous val)
