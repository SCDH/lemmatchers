module Main where

import Matchers
import TagRecords
import Data.Csv
import Data.String.Encode
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import System.FilePath

readMatchers :: FilePath -> IO Matchers
readMatchers = fmap read . readFile

readRecords :: FilePath -> IO [LemmaRecord]
readRecords = fmap (either fail (V.toList . snd) . decodeByName) . BS.readFile

writeResults :: [Match] -> FilePath -> IO ()
writeResults ms fp = BS.writeFile fp $ encodeByName hdr ms
  where hdr = V.fromList $ map convertString $ words
              "matcher pattern id_text Designation lemma"

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
