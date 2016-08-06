
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

drawSolution :: FilePath -> IO (Diagram B)
drawSolution path = do
    problem <- parseProblem path
    case problem of
      Problem [polygon] _ -> do 
         let polygon = head (pSilhouette problem)
             hull = convexHull polygon
         runSimpleSolver hull drawSvg printSolution
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
