module Main where

import Matchers
import TagRecords
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import System.FilePath

readMatchers :: FilePath -> IO Matchers
readMatchers = fmap read . readFile

readRecords :: FilePath -> IO [LemmaRecord]
readRecords = fmap (either fail (V.toList . snd) . decodeByName) . BS.readFile

main :: IO ()
main = do
  let dir = "data"
  matchers  <- readMatchers $ dir </> "matchers.txt"
  records   <- readRecords  $ dir </> "data.csv"
  -- TODO
  let lemmi = concatMap (lemmas . lemmata) records
  putStrLn $ show (length records) ++ " records with " ++ show (length lemmi) ++ " found."
