module ConvexHull where

-- https://gist.github.com/ashaindlin/c93f7ea6cab773f97103

import Data.List (sortBy, delete)
import Data.Ord (comparing)

import Problem

data Direction = Left | Right | Straight
    deriving (Show, Eq)

-- Calculate the turn made by three 2D points
getDirection :: Point -> Point -> Point -> Direction
getDirection a b c
    | turn > 0 = ConvexHull.Left
    | turn == 0 = ConvexHull.Straight
    | turn < 0 = ConvexHull.Right
    where turn = (((fst b)-(fst a))*((snd c)-(snd a))) - (((snd b)-(snd a)))*(((fst c)-(fst a)))

-- Given a list of points, list the directions of turns for them
path :: [Point] -> [Direction]
path (a:b:c:ps) = (getDirection a b c):(path (b:c:ps))
path _ = []

-- Starts in the +x direction and sweeps out a complete counter-clockwise circle
angleFromX :: Point -> Double
angleFromX p
    | angle >= 0 = angle
    | otherwise = angle + 2*pi
    where angle = atan2 (realToFrac $ snd p) (realToFrac $ fst p)

-- Sort points by angle from x-axis and duplicate the first one at end of list
preparePoints :: [Point] -> [Point]
preparePoints ps = sorted ++ [head sorted]
    where sorted = sortBy (comparing angleFromX) ps

convexHull :: Polygon -> Polygon
convexHull shape = (head ps):(map snd $ filter turn $ zip (path ps) (trim ps))
    where
        ps = preparePoints shape
        turn (d, _) = d /= ConvexHull.Right
        trim xs = init $ drop 1 xs

