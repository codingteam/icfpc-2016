
module Solver where

import Control.Monad
import Control.Monad.State
import Data.List
import System.FilePath
import System.FilePath.Glob

import Problem
import Transform
import Parser

-- import Debug.Trace

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
  in trace ("Fold Left: Cut <" ++ formatPolygon p ++ "> with <" ++ formatSegment seg ++ ">:\n\t<" ++ 
            formatPolygon p1 ++ ">\n\t<" ++ formatPolygon p2 ++ ">") $
      if null p2
        then [(FoldLeft seg: ts, {-flipPolygon seg-} p1)]
        else if null p1
             then [(ts, p2)]
             else [(FoldLeft seg: ts, {-flipPolygon seg-} p1), (ts, p2)]

foldPolygonRight :: Segment -> TransformedPolyon -> [TransformedPolyon]
foldPolygonRight seg (ts, p) =
  let (p1,p2) = cutPolygon seg p
  in trace ("Fold Right: Cut <" ++ formatPolygon p ++ "> with <" ++ formatSegment seg ++ ">:\n\t<" ++ 
            formatPolygon p1 ++ ">\n\t<" ++ formatPolygon p2 ++ ">") $
      if null p2
        then [(ts, p1)]
        else if null p1
             then [(FoldRight seg: ts, {-flipPolygon seg-} p2)]
             else [(ts, p1), (FoldRight seg: ts, {-flipPolygon seg-} p2)]

doFoldLeft :: Segment -> Solver ()
doFoldLeft seg = do
  modify $ \polygons -> concatMap (foldPolygonLeft seg) polygons

doFoldRight :: Segment -> Solver ()
doFoldRight seg = do
  modify $ \polygons -> concatMap (foldPolygonRight seg) polygons

doAutoFold :: Point -> Segment -> Solver ()
doAutoFold ctr seg =
  case ctr `relativeTo` seg of
    OnLeft -> trace ("Fold Right around " ++ formatSegment seg) $ doFoldRight seg
    _ -> trace ("Fold Left around " ++ formatSegment seg) $ doFoldLeft seg

unfoldPolygon :: TransformedPolyon -> Polygon
unfoldPolygon (transforms, p) = applyTransform (reverse transforms, p)

applyTransform :: TransformedPolyon -> Polygon
applyTransform (transforms, p) = go transforms p
  where
    go [] p = p
    go (t:ts) p = go ts $ apply t p

    apply (FoldLeft seg) p = flipPolygon seg p
    apply (FoldRight seg) p = flipPolygon seg p

center :: Polygon -> Point
center poly = 
    let (sx,sy) = foldr plus (0,0) poly
        n = fromIntegral (length poly)
    in  (sx / n, sy / n)
  where
    plus (x1,y1) (x2,y2) = (x1+x2, y1+y2)

simpleSolve1 :: Polygon -> Solver ()
simpleSolve1 poly = do
  let edges = zip poly (tail poly) ++ [(last poly, head poly)]
      ctr = center poly
  forM_ edges $ \edge -> 
    --doFoldRight (elongate edge)
    --doFoldLeft edge
    doAutoFold ctr (elongate edge)

