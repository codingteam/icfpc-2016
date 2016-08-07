
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

formatNumber :: Number -> String
formatNumber x = go (numerator x) (denominator x)
  where 
    go n 1 = show n
    go n d = show n ++ "/" ++ show d

formatPoint :: Point -> String
formatPoint (x,y) = formatNumber x ++ "," ++ formatNumber y

formatPolygon :: Polygon -> String
formatPolygon poly = unwords $ map formatPoint poly

formatSegment :: Segment -> String
formatSegment (a,b) = formatPoint a ++ " -- " ++ formatPoint b

isInUnitSquare :: Polygon -> Bool
isInUnitSquare poly = all good poly 
  where
    good (x,y) =
      x >= 0 && x <= 1 &&
      y >= 0 && y <= 1
