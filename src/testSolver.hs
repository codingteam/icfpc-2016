{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad
import Control.Monad.State
import Data.Ratio
import Text.Parsec
import Text.Parsec.String
import Diagrams.Prelude hiding (unitSquare)
import qualified Diagrams.Prelude as DP
import Diagrams.Backend.SVG.CmdLine

import Problem
import Solver
import Solution
import Parser
import Draw

drawSolution :: FilePath -> IO (Diagram B)
drawSolution path = do
    problem <- parseProblem path
    if isSimpleProblem problem
      then do 
         let polygon = head (pSilhouette problem)
         runSimpleSolver polygon drawSvg printSolution (error "Simple solver failed")
      else fail $ "Problem is not so simple"
  where
    drawSvg :: [Polygon] -> Diagram B
    drawSvg unfoldedPolys = do
         mconcat $ map drawPolygon $ unfoldedPolys

    printSolution foldedPolys = 
         putStr $ formatSolution $ foldedPolys

main :: IO ()
main = do
  mainWith drawSolution

