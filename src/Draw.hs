
module Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Problem

mkPoint :: Problem.Point -> P2 Double
mkPoint (x,y) = p2 (realToFrac x, realToFrac y)

drawPolygon :: Polygon -> Diagram B
drawPolygon points =
  strokeLoop $ closeLine $ lineFromVertices $ map mkPoint $ points ++ [head points]

drawSegment :: Problem.Segment -> Diagram B
drawSegment (a,b) =
  fromVertices $ [mkPoint a, mkPoint b]

drawProblem :: Problem -> Diagram B
drawProblem (Problem polygons skeleton) =
  let dPolygons = foldr1 atop $ map drawPolygon polygons
      dSegments = foldr1 atop $ map drawSegment skeleton
  in  (dPolygons # fc grey) `atop` (dSegments # lc red)
