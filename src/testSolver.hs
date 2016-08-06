
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

main :: IO ()
main = do
  text <- getContents
  case parse pPolygon "<input>" text of
    Left err -> fail $ show err
    Right p -> do
      let initState = [([], unitSquare)]
      let foldedPolys = execState (simpleSolve1 p) initState
          unfoldedPolys = map unfoldPolygon foldedPolys
      let dgram = mconcat $ map drawPolygon $ map snd foldedPolys
      {-forM_ foldedPolys $ \(ts, p) -> do
        putStrLn (formatPolygon p)-}
      mainWith dgram
      putStrLn $ formatSolution $ foldedPolys

