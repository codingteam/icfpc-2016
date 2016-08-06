
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
import Parser
import Draw

solver :: Solver ()
solver = do
  doFoldLeft ((1%2, 0), (1%2, 1))
  doFoldLeft ((0, 1%2), (1, 1%2))

unitSquare :: Polygon
unitSquare = 
  [(0,0), (1,0), (1,1), (0,1)]

drawSolution :: FilePath -> IO (Diagram B)
drawSolution path = do
  problem <- parseProblem path
  if isSimpleProblem problem
    then do
         let p = head (pSilhouette problem)
         let initState = [([], unitSquare)]
         let foldedPolys = execState (simpleSolve1 p) initState
             unfoldedPolys = map applyTransform foldedPolys
         let dgram = mconcat $ map drawPolygon $ unfoldedPolys
         {-forM_ foldedPolys $ \(ts, p) -> do
           putStrLn (formatPolygon p)-}
         putStr $ formatSolution $ foldedPolys
         return dgram
    else fail $ "Problem is not so simple"

main :: IO ()
main = do
  mainWith drawSolution

