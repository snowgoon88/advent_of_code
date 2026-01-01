
module Main where

{- testing and demo of the MySolver module.

- gaussElimination, reduction of Gauss Eliminated Matrix
-}

import qualified MySolver as MS
import System.Environment (getArgs)

main :: IO ()
main = do
  print "==============================================="
  print "== Test of Solver methods                    =="
  print "==============================================="

  -- MS.VMat on the input line
  args <- getArgs
  if length args >= 1
    then do
        let mat = MS.mkMat (read (head args))
        testGE mat
    else MS.test01
  print "END ==========================================="

-- apply GaussElimination and (if possible) reduction
testGE :: MS.VMat -> IO ()
testGE mat = do
  putStrLn $ "mat *****\n" ++ MS.niceSystem mat
  putStrLn $ MS.tabMat mat

  let res = MS.gaussElimM mat 0
  print "** Gauss Elimination ******************"
  putStrLn $ either show MS.tabMat res
  print "** Reduction **************************"
  case res of
    Left _    -> print "No reduction possible"
    Right geMat -> do
      let red = MS.reduceMatrix geMat
      putStrLn $ MS.tabMat red


-- from https://en.wikipedia.org/wiki/Gaussian_elimination
sysGE = MS.mkMat [[2, 1, -1, 8], [-3, -1, 2, -11], [-2, 1, 2, -3]]
sysAoC2510_74 = MS.mkMat [ [0 ,  0 ,  1 ,  0 ,  0 ,  0 ,  1 ,  1 ,  1 , 28]
                         , [0 ,  0 ,  1 ,  1 ,  1 ,  1 ,  0 ,  0 ,  0 , 44]
                         , [1 ,  0 ,  0 ,  1 ,  1 ,  0 ,  0 ,  0 ,  0 , 38]
                         , [1 ,  0 ,  1 ,  1 ,  1 ,  0 ,  1 ,  0 ,  0 , 55]
                         , [0 ,  0 ,  1 ,  0 ,  0 ,  0 ,  1 ,  0 ,  1 , 20]
                         , [0 ,  1 ,  0 ,  0 ,  1 ,  0 ,  1 ,  0 ,  0 , 41]
                         , [1 ,  1 ,  0 ,  0 ,  1 ,  1 ,  1 ,  1 ,  1 , 67]
                         ]
