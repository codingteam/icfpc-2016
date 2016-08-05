
module Solver where

import Control.Monad
import Data.List
import System.FilePath
import System.FilePath.Glob

import Problem
import Transform
import Parser

isConvex :: Polygon -> Bool
isConvex points =
  let vectors = zipWith mkVector points (tail points) ++ [mkVector (last points) (head points)]
      mkVector (x1,y1) (x2,y2) = (x2-x1, y2-y1)
      vectorProduct (x1,y1) (x2,y2) = x1*y2 - x2*y1
      products = zipWith vectorProduct vectors (tail vectors) ++ [vectorProduct (last vectors) (head vectors)]
  in  all (>= 0) products || all (<= 0) products

isSimpleProblem :: Problem -> Bool
isSimpleProblem (Problem [polygon] _) = isConvex polygon
isSimpleProblem _ = False

findSimpleProblems :: FilePath -> IO ()
findSimpleProblems dir = do
  paths <- glob (dir </> "*.txt")
  forM_ paths $ \path -> do
    problem <- parseProblem path
    when (isSimpleProblem problem) $
      putStrLn $ takeFileName path
