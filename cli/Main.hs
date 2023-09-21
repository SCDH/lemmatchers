module Main where

import Lemmatchers.CLI
import System.Environment

main :: IO ()
main = do
  args    <- getArgs
  input   <- getContents
  output  <- run args input
  putStr output
