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
import qualified MySolver as MS

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
import Data.Char (digitToInt)
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.Foldable ( foldl' )
import Data.Foldable ( foldl', foldlM )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List ( intercalate, transpose, partition )
import Data.List.Extra ( splitOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
import qualified Data.Vector as V
import Data.Ratio
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)

-- *********************************************************************************** DEBUG
import Debug.Trace ( trace ) -- trace :: String > a -> a
import qualified MySolver as MS
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug :: c -> String -> c
debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2025 - Day 10ter Part - & 2                                         **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let ms = map parseMachine (lines content)
  mapM_ (\(i, m) -> do
           let sol = findMinimum m
           putStrLn $ "** " ++ show i ++ " ************************************"
           putStrLn $ "mach=" ++ show m
           putStrLn $ "sol=" ++ show sol
       ) (zip [0..] ms)

  -- let idDebug = 74
  -- findMinimumDebug (ms !! idDebug)
  -- -- print "** real solving..."
  -- let fm = findMinimum (ms !! idDebug)
  -- putStrLn $ "fm=" ++ show fm

  -- putStrLn $ "Answer 1> " ++ show pRes

  let cRes = sum (map findMinimum ms)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
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

-- *****************************************************************************
-- ********************************************************************** Part 2

-- findMinimum mach = allPossible             -- all to Try
-- findMinimum mach = map opSolve allPossible -- gives all solutions
findMinimum mach = minimum $ map sum $ filter (V.all (\v -> denominator v == 1 && v >= (0%1))) allSols
  where
    -- matrix of augmented system
    sys = MS.mkMat (machToAugmentedMat mach)
    -- Gauss Elimination can fail
    resGE = MS.gaussElimM sys 0
    reduced = case resGE of
      Left _ -> error ("No reduction possible of " ++ show mach)
      Right geMat -> MS.reduceMatrix geMat
    idNZCoef = getIdNZCoef reduced
    compChain = buildChain idNZCoef []
    csOrder = mkConstraintsOrder compChain []

    -- all possible values
    -- allIdVars = varIdx idNZCoef
    -- vecIni = V.replicate (Set.findMax allIdVars) (0%1)
    vecIni = V.replicate (length (m_btn mach)) (0%1)
    idToSet = cc_set (head compChain)
    ranges = map (mkRange sys) idToSet

    -- using sequence in the list Monad is cartesian product
    allPossible = mapM (enumFromTo (0%1)) ranges
    allSols = map opSolve allPossible -- `debug` ("__findMinimum: csOrder=" ++ show csOrder)

    -- solving constraints
    opSolve ratVals = solveConstraints reduced csOrder valSetted
      where
        valSetted = foldl' (\vec (i, val) -> setVec vec i val) vecIni
                           (zip idToSet ratVals)

findMinimumDebug mach = do -- minimum $ map sum $ filter (V.all (>= (0%1))) allSols
    putStrLn $ "mach=" ++ show mach
    -- matrix of augmented system
    let sys = MS.mkMat (machToAugmentedMat mach)
    print "__System"
    putStrLn $ MS.tabMat sys
    -- Gauss Elimination can fail
    let resGE = MS.gaussElimM sys 0
    case resGE of
      Left _ -> error ("No reduction possible of " ++ show mach)
      Right geMat -> do
        print "__Gauss Elimination"
        putStrLn $ MS.tabMat geMat
        -- let reduced = V.map ensureIntRow (MS.reduceMatrix geMat)
        let reduced = MS.reduceMatrix geMat
        print "__Reduction"
        putStrLn $ MS.tabMat reduced
        let idNZCoef = getIdNZCoef reduced
        let compChain = buildChain idNZCoef []
        let csOrder = mkConstraintsOrder compChain []
        putStrLn $ "idNZCoef=" ++ show idNZCoef
        putStrLn $ "compChain=" ++ show compChain
        putStrLn $ "csOrder=" ++ show csOrder

        -- all possible values
        -- let allIdVars = varIdx idNZCoef
        -- let vecIni = V.replicate (Set.findMax allIdVars) (0%1)
        let vecIni = V.replicate (length (m_btn mach)) (0%1)
        putStrLn $ "vecInit=" ++ show vecIni
        let idToSet = cc_set (head compChain)
        let ranges = map (mkRange sys) idToSet
        putStrLn $ "ranges=" ++ show ranges

        -- using sequence in the list Monad is cartesian product
        let allPossible = mapM (enumFromTo (0%1)) ranges
        let opSolve ratVals = solveConstraintsDebug reduced csOrder valSetted
              where
                valSetted = foldl' (\vec (i, val) -> setVec vec i val) vecIni
                            (zip idToSet ratVals)

        allSols <- mapM opSolve allPossible
        putStrLn $ "allSols=" ++ show allSols
        let filteredSols = filter (V.all (\v -> denominator v == 1 && v >= (0%1))) allSols
        putStrLn $ "filteredSols=" ++ show filteredSols


-- from Machine to Augmented Matrix compatibl with MySolver.algo
machToAugmentedMat :: Machine -> [[Integer]]
machToAugmentedMat mach = transpose $ map (map (toInteger . digitToInt)) (m_btn mach)
                                      ++ [map toInteger (m_jol mach)]

-- from Reduced Matrix, try to find "free" variable, begining from the "last"
-- one, i.e. the rightmost *variable*

type IdVarCoef = (Int, MS.RatNum)
type System = [(Int, [IdVarCoef])]

data ComputaionChain = CC { cc_set :: [Int]        -- id of variables to set
                          , cc_free :: Set.Set Int -- id of still free variables
                          , cc_sys :: System }     -- way to compute the values
  deriving (Show)

ensureIntRow :: MS.VRow -> MS.VRow
ensureIntRow row
  | commonDenom > 1 = V.map (* (commonDenom%1)) row
  | otherwise       = row
  where
    commonDenom = product $ Set.toList (Set.fromList (map denominator (V.toList row)))

-- build a list idRow -> decreasing list of non-zero indices (idCol, val)
-- opIdFree :: MS.VRow -> [(Int, MS.RatNum)œ
getIdNZCoef :: MS.VMat -> System
getIdNZCoef mat = filter (not . null . snd) (zip [0..] $ map (reverse . opRow) (V.toList mat))
  where
    opRow row = V.toList $ V.filter ((/= 0%1) . snd) (V.indexed (V.init row))

-- build a Chain of Computations to determine in which order to set
-- which variables
-- WARNING : computation order is to be reversed
buildChain :: System -> [ComputaionChain] -> [ComputaionChain]
buildChain [] comps = comps
buildChain sys [] = buildChain leftover [nextComp]
  where
    -- init Computation
    compInit = CC [] (varIdx sys) []
    -- oneStep
    (nextComp, leftover) = stepChain sys compInit
buildChain sys (comp:cs) = buildChain leftover (nextComp : comp : cs)
  where
    (maxFree, leftFree) = Set.deleteFindMax (cc_free comp)
    newComp = comp { cc_set = maxFree : cc_set comp
                   , cc_free = leftFree
                   , cc_sys = [] }
    -- oneStep
    (nextComp, leftover) = stepChain sys newComp

stepChain :: System -> ComputaionChain -> (ComputaionChain, System)
stepChain [] comp = (comp, [])
stepChain sys comp
  | null computable = (comp, leftover)
  | otherwise       = stepChain leftover newComp
  where
    idVar = Set.toList (cc_free comp)
    (computable, leftover) = separateValuable sys idVar
    newVar = varIdx computable
    newComp = comp { cc_free = Set.difference (cc_free comp) newVar
                   , cc_sys = cc_sys comp ++ computable }

-- a var is *valuable* if its value can be determined given the still
-- free variable
separateValuable :: System -> [Int] -> (System, System)
separateValuable sys idFree = partition (isValuable . snd) sys
  where
    isValuable varCoef = length (filter (flip elem idFree . fst) varCoef) == 1
-- all the varIndex of the system
varIdx :: System -> Set.Set Int
varIdx sys = Set.fromList $ concatMap (map fst . snd) sys

-- assess the range of a variable
-- between 0%1 and the min of positive target / coef_i for each row
mkRange :: MS.VMat -> Int -> MS.RatNum
mkRange mat idVar = max (0%1) (V.minimum (V.filter (> 0%1) values))
  where
    values = V.map opDiv mat
    opDiv vec
      | vec V.! idVar == 0%1  = 0%1
      | otherwise             = V.last vec / vec V.! idVar

-- set one value in a Vector
setVec :: MS.VRow -> Int -> MS.RatNum -> MS.VRow
setVec vec 0 val = V.cons val $ V.drop 1 vec
setVec vec idx val = V.take idx vec V.++ V.cons val (V.drop (idx+1) vec)

-- make a chain of Constraint use for quicker computation
mkConstraintsOrder :: [ComputaionChain] -> [([Int], (Int, [IdVarCoef]))] -> [([Int], (Int, [IdVarCoef]))]
mkConstraintsOrder [] constraints = constraints
mkConstraintsOrder (comp:cs) constraints = mkConstraintsOrder cs (newConst ++ constraints)
  where
    idVars = cc_set comp
    newConst = map (\ic -> (idVars, ic)) (cc_sys comp)
-- chainComputation sys (c:cs) vec

solveConstraints :: MS.VMat -> [([Int], (Int, [IdVarCoef]))]-> MS.VRow -> MS.VRow
solveConstraints sys cons vec = fst $ foldl' (useConstraint sys) (vec, []) cons
useConstraint sys (vec, setVars) (idVars, (idRow, coefs))
  | idNew < 0 = error ("ERROR useConstraint: idNew < 0 WITH idVar=" ++ show idVars ++ " idRow=" ++ show idRow ++ " coefs=" ++ show coefs)
  | idNew >= V.length vec = error ("ERROR useConstraint: idNew > size WITH idVar=" ++ show idVars ++ " idRow=" ++ show idRow ++ " coefs=" ++ show coefs)
  | otherwise = (setVec vec idNew (valNew / ((sys V.! idRow) V.! idNew )), idNew : newSetVars) -- , valIni, idNew, valNew)
  where
    valIni = V.last (sys V.! idRow) -- `debug` ("__useConstraint: WITH idVar=" ++ show idVars ++ " idRow=" ++ show idRow ++ " coefs=" ++ show coefs)
    newSetVars = setVars ++ idVars
    (idNew, valNew) = foldl' opAddVar (-1, valIni) coefs
    opAddVar (idn, acc) (idCol, coef)
      | elem idCol newSetVars = (idn, acc - (vec V.! idCol) * coef)
      | otherwise = (idCol, acc)

-- cons :: [([Int], (Int, [IdVarCoef]))]
solveConstraintsDebug sys cons vec = do
  putStrLn $ "***** Solving with " ++ show vec
  res <- foldlM (useConstraintDebug sys) (vec, []) cons
  putStrLn $ "  res=" ++ show res
  return (fst res)

useConstraintDebug sys (vec, setVars) (idVars, (idRow, coefs)) = do
  putStrLn $ "__useConstraint: WITH idVar=" ++ show idVars ++ " idRow=" ++ show idRow ++ " coefs=" ++ show coefs
  putStrLn $ "                 vec=" ++ show vec ++ " setVars=" ++ show setVars
  let valIni = V.last (sys V.! idRow)
  let newSetVars = setVars ++ idVars
  let (idNew, valNew) = foldl' opAddVar (-1, valIni) coefs
        where
          opAddVar (idn, acc) (idCol, coef)
            | elem idCol newSetVars = (idn, acc - (vec V.! idCol) * coef)
            | otherwise = (idCol, acc)
  -- multiply by invert
  let res
        | idNew < 0 = error "ERROR idNew < 0"
        | idNew >= V.length vec = error "ERROR idNew > size"
        | otherwise = setVec vec idNew (valNew / ((sys V.! idRow) V.! idNew ))
  return (res, idNew:newSetVars)

