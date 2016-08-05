
module Solver where

import Control.Monad
import Control.Monad.State
import Data.List
import System.FilePath
import System.FilePath.Glob

import Problem
import Transform
import Parser

import Debug.Trace

type TransformedPolyon = ([Fold], Polygon)
type SolverState = [TransformedPolyon]
data Fold = 
    FoldLeft Segment
  | FoldRight Segment
  deriving (Eq, Show)

type Solver a = State SolverState a

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

elongate :: Segment -> Segment
elongate ((x1,y1), (x2,y2)) = 
  if x1 == x2
    then ((x1, 0), (x1, 1))
    else if y1 == y2
           then ((0,y1), (1, y1))
           else let k = (y2-y1) / (x2-x1)
                    b = y1 - k*x1
                in if k > 1
                     then let x1' = -b/k
                              x2' = (1-b)/k
                          in  ((x1', 0), (x2', 1))
                     else let y1' = b
                              y2' = k+b
                          in ((0,y1'), (1, y2'))

foldPolygonLeft :: Segment -> TransformedPolyon -> [TransformedPolyon]
foldPolygonLeft seg (ts, p) =
  let (p1,p2) = cutPolygon seg p
  in  if null p2
        then [(FoldLeft seg: ts, p1)]
        else if null p1
             then [(FoldLeft seg: ts, p2)]
             else [(FoldLeft seg: ts, p1), (FoldLeft seg: ts, p2)]

doFoldLeft :: Segment -> Solver ()
doFoldLeft seg = do
  modify $ \polygons -> concatMap (foldPolygonLeft seg) polygons

simpleSolve1 :: Solver ()
simpleSolve1 = do
  [(_, poly)] <- get
  let edges = zip poly (tail poly) ++ [(last poly, head poly)]
  forM_ edges $ \edge -> 
    doFoldLeft (traceShowId $ elongate edge)

