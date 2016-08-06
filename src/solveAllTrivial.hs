
module Main where

import Control.Monad
import System.FilePath
import System.FilePath.Glob
import System.Environment

import Trivial

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ fail "Synopsis: solveAllTrivial problems/ solutions/"
  let [srcDir, dstDir] = args
  inFiles <- glob (srcDir </> "*.txt")
  forM_ inFiles $ \inFile -> do
    let outFile = dstDir </> takeFileName inFile
    runTrivialSolver inFile outFile

