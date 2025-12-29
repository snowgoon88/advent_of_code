module MySolver ( RatNum, invert, niceRat
                , VRow, VCol, VMat, mkRow, mkMat
                , getCol, tabMat, swapRow, mulRow, addMulRow
                , gaussElimM, reduceMatrix
                , niceConstraint, niceSystem
                , test01) where

{-
Usage example (see also TestSolver.hs)

testGE mat = do
  putStrLn $ "mat *****\n" ++ niceSystem mat
  putStrLn $ tabMat mat

  let res = gaussElimM mat 0
  print "** Gauss Elimination ******************"
  putStrLn $ either show tabMat res

  print "** Reduction **************************"
  case res of
    Left _    -> print "No reduction possible"
    Right geMat -> do
      let red = reduceMatrix geMat
      putStrLn $ tabMat red
-}

import Data.List ( intercalate )
import qualified Data.Vector as V
import Data.Ratio

-- *****************************************************************************
-- Rational Number are made of Integer numerator and denominator
type RatNum = Data.Ratio.Rational

-- get the invert of a Rational Number, or Not...
invert :: RatNum -> Maybe RatNum
invert r
  | numerator r /= 0 = Just (denominator r % numerator r)
  | otherwise = Nothing

niceRat :: (Eq a, Num a, Show a) => Ratio a -> String
niceRat r
  | denominator r == 1 = show $ numerator r
  | otherwise          = show (numerator r) ++ "/" ++ show (denominator r)

-- *****************************************************************************
-- Matrix are Vector of VRow
type VRow = V.Vector RatNum
type VCol = V.Vector RatNum
type VMat = V.Vector VRow

mkRow :: [Integer] -> VRow
mkRow ns = V.generate (length ns) opMk
  where
    opMk idx = (ns !! idx) % 1

-- linear combination of x_i = val_i
-- last element of VRow is the val
niceConstraint :: VRow -> String
niceConstraint r = vars ++ equs
  where
    rList = V.toList r
    vars = intercalate " + " $ zipWith (\ i rat -> show rat ++ " . x" ++ show i) [1..] (init rList)
    equs = " = " ++ show (last rList)

mkMat :: [[Integer]] -> VMat
mkMat rs = V.generate (length rs) opMat
  where
    opMat idx = mkRow (rs !! idx)

getCol :: VMat -> Int -> VCol
getCol mat idxCol = V.map ( V.! idxCol ) mat

-- display VMat as lines of nice Constraints
niceSystem :: VMat -> String
niceSystem s = intercalate "\n" (map niceConstraint (V.toList s))

-- display VMat as lines of | int. coef | ... | int. coef |
tabMat :: VMat -> String
tabMat mat = V.foldl' (\out row -> out ++ V.foldl' (\msg s -> msg ++ " | " ++ padd maxLen s) "" row ++ "\n") "" matStr
  where
    matStr = V.map (V.map niceRat) mat
    matLen = V.map (V.map length) matStr
    maxLen = maximum (V.maximum matLen)
    padd :: Int -> String -> String
    padd n str
      | (n - length str) > 0 = replicate (n - length str) ' ' ++ str
      | otherwise          = str

-- *****************************************************************************
-- Gauss Elimination algorithm
-- => Either Left error_msg | Right row_echelon_form
gaussElimM :: VMat -> Int -> Either String VMat
gaussElimM mat idxStart
  | idxStart > length mat = Right mat
  | otherwise = do
      let res = stepGaussElimM mat idxStart
      case res of
        Left "no_nonzeros_columns" -> Right mat
        Right newMat               -> gaussElimM newMat (idxStart+1)
        Left err_msg               -> Left err_msg

-- find a non-zero subcolumn, starting at row 'idxStart'
-- => Either Left "no_zeros" | Right (idxNZCol, nzSubCol)
findNZcolumn :: VMat -> Int -> Either String (Int, VCol)
findNZcolumn mat idxStart = res
  where
    -- find first non-nul column
    subSize = length mat - idxStart
    nbCols = length (mat V.! 0)
    allSubCols = map (\i -> (i, V.slice idxStart subSize (getCol mat i))) [idxStart..nbCols-1]
    res = if not (any (\(_, col) -> V.any (/= 0%1) col) allSubCols)
      then Left "no_nonzeros_columns"
      else Right (head $ filter (\(_, col) -> V.any (/= 0%1) col) allSubCols)

-- swap system in order to bring the row with the leftmost NZ entry at
-- row idxStart
-- => Either Left "error_no_nz_elem" | Right swappedSystem
swapToNonZero :: VMat -> Int -> (Int, VCol) -> Either String VMat
swapToNonZero mat idxStart (idNZCol, nzSubCol) = case V.findIndex (/= (0%1)) nzSubCol of
  Just idxNZRow -> Right (swapRow mat idxStart (idxNZRow+idxStart))
  Nothing       -> Left "error_no_nz_elem"

-- set the idxStart leftmost NZ entry to 1
-- => Either Left "" | Right onedSystem
setToOne :: VMat -> Int -> Int -> Either String VMat
setToOne mat idxStart idNZCol = oneFirst
  where
    invMul = invert ((mat V.! idxStart) V.! idNZCol)
    oneFirst = case invMul of
      Just val -> Right $ mulRow mat idxStart val
      Nothing  -> Left "error_div_0"

-- eliminate the coef at idNZCol from all rows after idxStart
-- ==> Either Left never | Right elimSys
elim :: VMat -> Int -> Int -> Either String VMat
elim mat idxStart idNZCol = Right $ V.imap opElim mat
  where
    opElim i row
      | i > idxStart = V.zipWith (\v1 v2 -> v2 * n + v1) row (mat V.! idNZCol)
      | otherwise = row
      where
        n = - ((mat V.! i) V.! idNZCol)

stepGaussElimM :: VMat -> Int -> Either String VMat
stepGaussElimM mat idStart = do
  (idNZ, colNZ) <- findNZcolumn mat idStart
  swappedMat <- swapToNonZero mat idStart (idNZ, colNZ)
  onedMat <- setToOne swappedMat idStart idNZ
  elim onedMat idStart idNZ

-- debug version of stepGaussElim
stepGaussElimMDebug mat idStart
  | idStart >= length mat = print "error_idStart_too_large"
  | otherwise = do
      let nz = findNZcolumn mat idStart
      putStrLn $ "(idNZCol, nzSubCol)=" ++ show nz
      case nz of
        Left msg -> print "--> STOP Here"
        Right (idNZCol, nzSubCol) -> do
          let swappedMat = swapToNonZero mat idStart (idNZCol, nzSubCol)
          putStrLn $ "swapped=" ++ show swappedMat
          case swappedMat of
            Left msg -> print "--> STOP Here"
            Right smat -> do
              let onedMat = setToOne smat idStart idNZCol
              putStrLn $ "oned=" ++ show onedMat
              case onedMat of
                Left msg -> print "--> STOP Here"
                Right omat -> do
                  let res = elim omat idStart idNZCol
                  putStrLn $ "elim=" ++ show res

-- *****************************************************************************
-- Reducing a row echelon form
reduceMatrix :: VMat -> VMat
reduceMatrix mat = V.imap (reducRow mat) mat

reducRow :: VMat -> Int -> VRow -> VRow
reducRow mat idRow row = V.foldl' opReduceRow row subRows
  where
    subRows = V.drop (idRow+1) mat

opReduceRow :: VRow -> VRow -> VRow
opReduceRow curRow subRow = case findNZinRow subRow of
  Nothing -> curRow
  Just (idNZ, valSub) -> V.zipWith (\v1 v2 -> v2 * valMul + v1) curRow subRow
    where
      valMul = - ((curRow V.! idNZ) / valSub)

findNZinRow :: VRow -> Maybe (Int, RatNum)
findNZinRow row = case V.findIndex (/= (0%1)) row of
    Just idx -> Just (idx, row V.! idx)
    Nothing -> Nothing

-- operations on Matrix
swapRow :: VMat -> Int -> Int -> VMat
swapRow mat i1 i2 = V.imap opSwap mat
  where
    opSwap i row
      | i == i1   = mat V.! i2
      | i == i2   = mat V.! i1
      | otherwise = row

mulRow :: VMat -> Int -> RatNum -> VMat
mulRow mat idx n = V.imap opMul mat
  where
    opMul i row
      | i == idx  = V.map (*n) row
      | otherwise = row

addMulRow :: VMat -> Int -> Int -> RatNum -> VMat
addMulRow mat idTo idFrom n = V.imap opAddMul mat
  where
    opAddMul i row
      | i == idTo  = V.zipWith (\v1 v2 -> v2 * n + v1) row (mat V.! idFrom)
      | otherwise = row

-- *****************************************************************************
-- different case (success and errors)
test01 :: IO ()
test01 = do
  print "** Test01 ***************************************************"
  let mat = mkMat [[1, 1, 1], [0, 0, 0]]
  putStrLn $ niceSystem mat
  putStrLn $ "__stepGaussElim mat 1 SHOULD say 'no_nonzero_columns'"
  stepGaussElimMDebug mat 1
  putStrLn $ "__swapToNonZero mat 1 (1, subCol) SHOULD say 'error'"
  print (swapToNonZero mat 1 (1, V.slice 1 1 (getCol mat 1)))
  putStrLn $ "__setToOne mat 1 1 SHOULD say 'error'"
  print (setToOne mat 1 1)
