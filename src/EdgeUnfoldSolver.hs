module EdgeUnfoldSolver where

import Data.List

import Problem

findSharedEdges :: [Polygon] -> [(Segment, [Polygon])]
findSharedEdges polygons =
  map (\s -> (s, filter (containsSegment s) polygons)) $ nub $ concat $ map toSegments polygons
  where
    toSegments [] = []
    toSegments input@(point : _) = recMakeSegments (input ++ [point])
    recMakeSegments [] = []
    recMakeSegments (_ : []) = []
    recMakeSegments (point : points) = (point, (head points)) : recMakeSegments points
    containsSegment segment polygon = segment `elem` toSegments polygon

findUnfoldCandidates :: [Point] -> [Segment] -> [Segment]
findUnfoldCandidates vertices segments =
  filter (isUnwrapCandidate vertices) segments
  where
  isUnwrapCandidate vertices segment = do
    let distances = map (distanceFromLine segment) vertices
    let signs = map signum $ filter (/= 0) distances
    and $ map (== head signs) (tail signs)
  distanceFromLine segment vertex = do
    let ((x0, y0), (x1, y1)) = segment
    let (x2, y2) = vertex
    (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)
