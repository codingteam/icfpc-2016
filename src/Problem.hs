
module Problem where

import Data.Ratio

type Number = Ratio Integer
type Point = (Number, Number)

data Problem = Problem {
  pSilhouette :: Silhouette,
  pSkeleton   :: Skeleton
  }
  deriving (Eq, Show)

type Polygon = [Point]
type Segment = (Point, Point)
type Skeleton = [Segment]
type Silhouette = [Polygon]
