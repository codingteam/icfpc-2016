module EdgeUnfoldSolver where

import Control.Arrow
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Debug.Trace

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

-- | Split @skeleton@ in two parts, one of which will be on the left, another
-- on the right of the given @segment@ when we unfold the @skeleton@.
--
-- The parts aren't complete, closed skeletons! You should immediately merge
-- them with @mergeSkeletons@.
findFacets :: Skeleton -> Segment -> [Skeleton]
findFacets skeleton (startPt, endPt) = go graph startPt []
  where
  graph1 = M.fromListWith S.union $ map (\(k, v) -> (k, S.singleton v)) skeleton
  graph2 = M.fromListWith S.union $ map (\(v, k) -> (k, S.singleton v)) skeleton
  graph = M.unionWith S.union graph1 graph2

  go edges start facet
    | M.null edges = [reverse facet]
    | start == endPt =
        -- avoid returning the segment we were given
        if length facet > 1 then [reverse facet] else []
    | otherwise =
        let next = M.lookupDefault S.empty start edges
            edges' = M.delete start edges

            facets = map (\end -> (end, (start, end):facet)) (S.toList next)
        in  concatMap (\(start', facet') -> go edges' start' facet') facets
