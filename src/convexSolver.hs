
module Main where

import Control.Monad
import Control.Monad.State
import Data.Ratio
import Text.Parsec
import Text.Parsec.String
import Diagrams.Prelude hiding (unitSquare)
import Diagrams.Backend.SVG.CmdLine

import Problem
import Solver
import Solution
import ConvexHull
import Parser
import Draw

simplify :: Polygon -> Polygon
simplify poly = map go poly
  where
    go (x,y) = (rnd x, rnd y)
    rnd x = x `approxRational` (1 % 1000000000000)

drawSolution :: FilePath -> IO (Diagram B)
drawSolution path = do
    problem <- parseProblem path
    case problem of
      Problem [polygon] _ -> do 
         let polygon = head (pSilhouette problem)
             hull = convexHull polygon
             simplified = simplify hull
         runSimpleSolver simplified drawSvg printSolution (error "Simple solver failed")
      _ -> fail $ "Problem is not so simple"
  where
    drawSvg :: [Polygon] -> Diagram B
    drawSvg unfoldedPolys = do
         mconcat $ map drawPolygon $ unfoldedPolys

    printSolution foldedPolys = 
         putStr $ formatSolution $ foldedPolys

main :: IO ()
main = do
  mainWith drawSolution
