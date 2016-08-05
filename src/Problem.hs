
module Problem where

import Data.Ratio

type Number = Ratio Integer
type Point = (Number, Number)

data Problem = Problem {
  pPolygons :: [Polygon],
  pSkeleton :: [Segment]
  }
  deriving (Eq, Show)

type Polygon = [Point]
type Segment = (Point, Point)
