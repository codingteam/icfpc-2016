module Main where

import Control.Monad
import Data.Graph.AStar
import Data.List
import Data.Maybe
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (distance, Point)
import System.Environment

import qualified Data.HashSet as S

import Draw
import EdgeUnfoldSolver
import Parser
import Problem
import Transform

main :: IO ()
main = do
  [path] <- getArgs
  problem <- parseProblem path

  let skel = pSkeleton problem
  let points = nub . concat $ map (\(p1, p2) -> [p1, p2]) skel
  let unfolds = findUnfoldCandidates points skel
  let facets = map (findFacets skel) unfolds
  let facetPairs = zip unfolds (map genPairs facets)

  let haganeFacets = facetsFromSkeleton skel
  let sharedEdges = findSharedEdges haganeFacets
  forM_ haganeFacets $ \polygon -> do
    putStrLn $ unwords [ show   polygon
                       , show $ isFreePolygon polygon sharedEdges]

  forM_ (zip [1..] facetPairs) $ \(i, (divider, pairs)) -> do
    forM_ (zip [1..] pairs) $ \(j, (one, another)) -> do
      let one' = flipSkeleton divider one
      let another' = flipSkeleton divider another

      renderSVG
        ("pair_" ++ show i ++ "_" ++ show j ++ ".svg")
        (mkWidth 500)
        (frame 0.5
          (        ((# lc green) $ foldr1 atop $ map drawSegment one)
            `atop` ((# lc green) $ (# dashingO [1] 2) $ foldr1 atop $ map drawSegment one')
            `atop` ((# lc red)   $ foldr1 atop $ map drawSegment another)
            `atop` ((# lc red)   $ (# dashingO [1] 2) $ foldr1 atop $ map drawSegment another')
            `atop` ((# lc black) $ drawSegment divider)))

  let solutions = getCandidates (pSkeleton problem)
  putStrLn $ unwords ["Found", show $ length solutions, "solutions"]
  {-
  forM_ (zip [1..] (S.toList solutions)) $ \(i, solution) -> do
      renderSVG
        ("result_" ++ show i ++ ".svg")
        (mkWidth 500)
        (frame 2 ((# lc red) $ foldr1 atop $ map drawSegment solution))
  -}

for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Generates all possible next state of a skeleton by unfolding a single fold
-- in all places where it's possible.
getCandidates :: Skeleton -> S.HashSet Skeleton
getCandidates skel =
  let points = nub . concat $ map (\(p1, p2) -> [p1, p2]) skel
      unfolds = findUnfoldCandidates points skel
      facets = map (findFacets skel) unfolds
      facetPairs = zip unfolds (map genPairs facets)
      flipped = concat $
                for facetPairs $ \(divider, pairs) ->
                  concat $
                  for pairs $ \(one, another) ->
                    [ (flipSkeleton divider one, another)
                    , (one, flipSkeleton divider another)
                    ]
      merged = map (uncurry mergeSkeletons) flipped
  in  S.fromList merged

-- | Given all possible facets present in initial skeleton, generate pairs of
-- skeletons that, when combined, will amount to initial skeleton.
genPairs :: [Skeleton] -> [(Skeleton, Skeleton)]
genPairs (x:y:xs) =
  (x, foldl' mergeSkeletons y xs) : genPairs ((mergeSkeletons x y):xs)
genPairs _ = []

-- | This is always applied to two neighboring states, so we can be totally
-- sure the difference between them is one single unfold.
distance :: Skeleton -> Skeleton -> Integer
distance _ _ = 1

heuristic :: Skeleton -> Integer
heuristic skel = 0

goal :: Skeleton -> Bool
goal skel = ((length skel) == 4)

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt $ fromRational $ (x1 - x2)^2 + (y1 - y2)^2
