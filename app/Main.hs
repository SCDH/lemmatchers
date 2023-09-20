module Main where

import Lemmatchers.Matchers
import Lemmatchers.TagRecords
import qualified Data.ByteString.Lazy as BS
import System.FilePath

readMatchers :: FilePath -> IO Matchers
readMatchers fp = read <$> readFile fp

readRecords :: FilePath -> IO [LemmaRecord]
readRecords fp = csvToLemmaRecords <$> BS.readFile fp

writeResults :: [Match] -> FilePath -> IO ()
writeResults ms fp = BS.writeFile fp $ matchesToCsv ms

main :: IO ()
main = do

  -- Load data
  let dir = "data"
  recordMatchers  <- readMatchers $ dir </> "matchers.txt"
  records         <- readRecords  $ dir </> "data.csv"

  -- Report data summary
  let lemmi = concatMap (lemmas . lemmata) records
  putStrLn $ show (length records) ++ " records with "
          ++ show (length lemmi) ++ " found."

  -- Match!
  let results = concat  [ matchRecord m lr
                          | m   <- matchers recordMatchers
                          , lr  <- records
                        ]

  -- Report!
  let output = dir </> "results.csv"
  putStrLn $ show (length results) ++ " results found."
  writeResults results $ output
  putStrLn $ "Output written to " ++ output
