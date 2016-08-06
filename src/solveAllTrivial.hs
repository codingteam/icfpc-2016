
module Main where

import Control.Monad
import System.FilePath
import System.FilePath.Glob
import System.Environment

import Trivial

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 3) $ fail "Synopsis: solveAllTrivial done.txt problems/ solutions/"
  let [donePath, srcDir, dstDir] = args
  done <- lines `fmap` readFile donePath
  let doneTxt = map (\n -> "problem_" ++ n ++ ".txt") done
  print doneTxt
  inFiles <- glob (srcDir </> "*.txt")
  forM_ inFiles $ \inFile -> do
    let basename = takeFileName inFile
    if basename `elem` doneTxt
      then putStrLn $ inFile ++ ": already done"
      else do
          let outFile = dstDir </> basename
          runTrivialSolver inFile outFile

