module Main where

import TagRecords
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V


-- Control Flow ---------------

readRecords :: FilePath -> IO [LemmaRecord]
readRecords = fmap (either fail (V.toList . snd) . decodeByName) . BS.readFile

main :: IO ()
main = do
  records <- readRecords "data/data.csv"
  let lemmi = concatMap (lemmas . lemmata) records
  putStrLn $ show (length records) ++ " records with " ++ show (length lemmi) ++ " found."
