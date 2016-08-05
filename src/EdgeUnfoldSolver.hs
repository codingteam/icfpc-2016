module EdgeUnfoldSolver where

import Problem

findUnfoldCandidates :: [Point] -> [Segment] -> [Segment]
findUnfoldCandidates vertices segments =
    filter (isUnwrapCandidate vertices) segments

isUnwrapCandidate :: [Point] -> Segment -> Bool
isUnwrapCandidate vertices segment = do
    let distances = map (distanceFromLine segment) vertices
    let signs = map signum $ filter (/= 0) distances
    and $ map (== head signs) (tail signs)

distanceFromLine :: Segment -> Point -> Number
distanceFromLine segment vertex = do
    let ((x0, y0), (x1, y1)) = segment
    let (x2, y2) = vertex
    (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)
