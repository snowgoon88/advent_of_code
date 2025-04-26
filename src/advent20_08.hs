{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- import Control.Monad (replicateM)
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Set as Set
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
-- ****** Data.List.Extra: splitOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 08 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input08.txt"
  -- content <- readFile "Input20/test08_1.txt"
  -- content2 <- readFile "Input20/test08_2.txt"
  -- let prog2 = Map.fromList $ zip [0..] (map parseInstruction (lines content2))
  -- content3 <- readFile "Input20/test08_3.txt"
  -- let prog3 = Map.fromList $ zip [0..] (map parseInstruction (lines content3))

  let prog = Map.fromList $ zip [0..] (map parseInstruction (lines content))
  -- putStrLn $ "prog=" ++ show prog

  -- let ex1 = St.runState (run ("acc", 3)) $ M 0 0
  -- putStrLn $ "ex1=" ++ show ex1

  -- let ex2 = do
  --       a1 <- run ("acc", 3)
  --       a2 <- run ("jmp", 2)
  --       return [a1, a2]
  -- putStrLn $ "ex2=" ++ show (St.runState ex2 $ M 0 0)

  -- let ex3 = mapM run [("acc", 3), ("jmp", 2)]
  -- putStrLn $ "ex3=" ++ show (St.runState ex3 $ M 0 0)

  -- let ex4 = sequence [run ("acc", 3), run ("jmp", 2)]
  -- putStrLn $ "ex4=" ++ show (St.runState ex4 $ M 0 0)

  -- let pr1 = St.runState (replicateM 5 (step prog)) $ M 0 0
  -- putStrLn $ "pr1=" ++ show pr1
  -- let pr1e = St.evalState (replicateM 5 (step prog)) $ M 0 0
  -- putStrLn $ "pr1e=" ++ show pr1e
  -- let pr1x = St.execState (replicateM 5 (step prog)) $ M 0 0
  -- putStrLn $ "pr1x=" ++ show pr1x

  let sol1 = solve1 [] prog (M 0 0)
  putStrLn $ "sol1=" ++ show sol1
  let pRes = mAcc sol1
  putStrLn $ "Answer 1> " ++ show pRes

  -- let term1 = terminate [] prog (M 0 0)
  -- putStrLn $ "term1=" ++ show term1
  -- let term2 = terminate [] prog2 (M 0 0)
  -- putStrLn $ "term2=" ++ show term1
  -- let term1 = terminate [] prog3 (M 0 0)
  -- putStrLn $ "term3=" ++ show term1

  -- let alter = listAlter prog
  -- putStrLn $ "alter=" ++ show alter

  let cRes = head $ solve2 prog
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Program = Map.Map Int Instruction
data Machine = M { mAdd :: Int, mAcc :: Int }
  deriving Show
type Instruction = (String, Int)
-- data Instruction = I { iOp :: String, iArg :: Int }
--   deriving (Show)

readArg :: String -> Int
readArg str = (read . dropWhile (=='+')) str

parseInstruction :: String -> Instruction
parseInstruction str = (op, (readArg arg))
  where
    [op,arg] = words str

-- s -> (a, s)
-- run :: Instruction -> St.State Machine Int
-- run inst = St.state (\s -> let news = apply inst s
--                                       in (mAdd news, news))
apply :: Instruction -> Machine -> Machine
apply ("nop", _) mach = mach { mAdd = (mAdd mach + 1) }
apply ("acc", val) mach = mach { mAdd = (mAdd mach + 1), mAcc = (mAcc mach + val) }
apply ("jmp", val) mach = mach { mAdd = (mAdd mach + val) }
apply _ mach = mach

step :: Monad m => Map.Map Int Instruction -> St.StateT Machine m Int
step programme = St.state (\machine ->
                             let new_machine = apply (programme Map.! (mAdd machine)) machine
                                 add = mAdd new_machine
                             in (add, new_machine)
                          )

-- npDouble :: Eq a => [a] -> Bool
-- noDouble [] = True
-- noDouble ls = not $ elem e le
--   where (e:le) = reverse ls
solve1 :: [Int] -> Map.Map Int Instruction -> Machine -> Machine
solve1 prevAdd prog mach
  | not (elem add prevAdd) = solve1 (add:prevAdd) prog new_mach
  | otherwise = new_mach
  where
    (add, new_mach) = St.runState (step prog) mach
-- stepSafe prog = do
--   mach <- get
--   let new_machine = apply (prog Map.! (smAdd mach)) mach
--   if elem (smAdd new_machine) (smPrev mach)
--     then
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

terminate ::
  [Int] -> Map.Map Int Instruction -> Machine -> Maybe Int
terminate prevAdd prog mach
  | (mAdd mach) == Map.size prog = Just (mAcc mach)
  | elem add prevAdd = Nothing
  | otherwise = terminate (add:prevAdd) prog new_mach
  where
    (add, new_mach) = St.runState (step prog) mach

listAlter :: Map.Map k (String, b) -> [k]
listAlter prog = Map.keys $ Map.filter (\a -> fst a /= "acc") prog

solve2 :: Map.Map Int (String, Int) -> [Maybe Int]
solve2 prog  = dropWhile (== Nothing) $
  map (\add -> terminate [] (switch prog add) (M 0 0)) (listAlter prog)
  where
    alt "jmp" = "nop"
    alt "nop" = "jmp"
    alt op = op
    switch pr i = Map.adjust (\(op, val) -> (alt op, val)) i pr

-- *****************************************************************************
-- *********************************************************************** SMART
-- *****************************************************************************

-- Smart is centered on a runCmd :: Program -> Machine -> Maybe Machine
-- fmap applied to the Maybe Functor, resulting from Map.lookup
runCmd :: Map.Map Int Instruction -> Machine -> Maybe Machine
runCmd prog mach = fmap (\cmd -> apply cmd mach) $ Map.lookup (mAdd mach) prog

-- make use of this function that returns the first repeated projection (by; b -> &)
firstRepeatedBy :: Ord a => (b -> a) -> [b] -> Maybe b
firstRepeatedBy f = go Set.empty
  where
    go seen (x:xs)
      | f x `Set.member` seen = Just x
      | otherwise           = go (f x `Set.insert` seen) xs
    go _ []     = Nothing

-- so, using iterrateMaybe that goes on until Nothing
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = go
  where
    go x = x : case f x of
      Nothing -> []
      Just y  -> go y

part1 :: Program -> Maybe Machine
part1 prog = firstRepeatedBy mAdd states
  where
    states = iterateMaybe (runCmd prog) $ M 0 0
