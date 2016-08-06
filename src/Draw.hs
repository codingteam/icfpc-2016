
module Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Debug.Trace as T

import Problem

mkPoint :: Problem.Point -> P2 Double
mkPoint (x,y) = p2 (realToFrac x, realToFrac y)

neg :: Problem.Point -> Problem.Point
neg (x, y) = (-x, -y)

drawPolygon :: Polygon -> Diagram B
drawPolygon points =
  let pts = points ++ [head points]
      color = if ccw pts then white else gray
  in showOrigin $ (moveOriginTo $ mkPoint $ neg $ head pts) $ fc color $ strokeLoop $ closeLine $ fromVertices $ map mkPoint pts
  where edges ps = zip ps $ tail ps
        ccw ps = (sum $ map (\((x1, y1), (x2, y2)) -> (x2 - x1) * (y2 + y1)) (edges ps)) > 0

drawSegment :: Problem.Segment -> Diagram B
drawSegment (a,b) =
  fromVertices $ [mkPoint a, mkPoint b]

drawProblem :: Problem -> Diagram B
drawProblem (Problem polygons skeleton) =
  let dPolygons = foldr1 atop $ map drawPolygon $ reverse polygons
      dSegments = foldr1 atop $ map drawSegment skeleton
  in  (dSegments # lc red) `atop` (dPolygons # fc grey) `atop` (unitSquare # translate (r2 (0.5,0.5)) # lc black)
