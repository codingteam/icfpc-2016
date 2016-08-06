module Printer where

import Data.List
import Data.Ratio
import Text.Printf (printf)

import Problem

strcat :: [String] -> String
strcat ss = concat $ intersperse "\n" ss

serializeNumber :: Number -> String
serializeNumber r | numerator r == 0 = "0"
serializeNumber r | denominator r == 1 = show $ numerator r
serializeNumber r = printf "%s/%s" (show $ numerator r) $ show $ denominator r

serializePoint :: Point -> String
serializePoint (x, y) = printf "%s,%s" (serializeNumber x) $ serializeNumber y

serializePolygon :: Polygon -> String
serializePolygon polygon = strcat $ [show $ length polygon] ++ map serializePoint polygon

serializeSegment :: Segment -> String
serializeSegment (x, y) = printf "%s %s" (serializePoint x) $ serializePoint y

serializePolygons :: [Polygon] -> String
serializePolygons ps = strcat $ map serializePolygon ps

serializeSegments :: [Segment] -> String
serializeSegments ss = strcat $ map serializeSegment ss

serializeProblem :: Problem -> String
serializeProblem (Problem silhouette skeleton) =
  strcat [(show $ length silhouette), (serializePolygons silhouette), (show $ length skeleton), (serializeSegments skeleton)]
