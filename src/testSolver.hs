
module Main where

import Control.Monad
import Control.Monad.State
import Data.Ratio
import Text.Parsec
import Text.Parsec.String
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Problem
import Solver
import Parser
import Draw

solver :: Solver ()
solver = do
  doFoldLeft ((1%2, 0), (1%2, 1))
  doFoldLeft ((0, 1%2), (1, 1%2))

main :: IO ()
main = do
  text <- getContents
  case parse pPolygon "<input>" text of
    Left err -> fail $ show err
    Right p -> do
      let initState = [([], p)]
      let rs = execState solver initState
      let dgram = mconcat $ map drawPolygon $ map snd rs
      forM_ rs $ \(ts, p) -> do
        print ts
        print p
      mainWith dgram
