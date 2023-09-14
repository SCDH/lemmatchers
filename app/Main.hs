module Main where

import qualified Data.ByteString.Lazy as BS
import Data.String.Encode
import qualified Data.Vector as V
import Data.Csv
import Text.Read
import Text.ParserCombinators.ReadP
import Control.Monad

-- All possible tags ----------

data Tag
  = AJ | AV | CNJ | DET | DN | DP | EN | GN | IP | J | LN
  | MN | MOD | N | NA | NU | PN | PRP | QN | QP | REL | RN
  | SN | TN | V | WN | XP
  deriving (Eq, Show, Read)

-- Lemma structure parsing ----

data Lemma = Lemma
  { lword         :: String
  , ltranslation  :: String
  , ltag          :: Tag
  } deriving (Eq, Show)

instance Read Lemma where
  readsPrec _ = readP_to_S parseLemma

parseLemma :: ReadP Lemma
parseLemma = do
  w <- munch1 (/= '[')
  t <- between (char '[') (char ']') $ munch (/= ']')
  (Just tag) <- readMaybe <$> munch (`elem` ['A'..'Z'])
  return $ Lemma w t tag

instance FromField Lemma where
  parseField = maybe mzero pure . readMaybe . getLenient . convertString

-- Lemma stream parsing -------

newtype Lemmas = Lemmas
  { lemmas :: [Lemma]
  } deriving (Eq, Show)

instance Read Lemmas where
  readsPrec _ = readP_to_S parseLemmas

parseLemmas :: ReadP Lemmas
parseLemmas = do
  ls <- sepBy parseLemma (char ' ')
  eof
  return $ Lemmas ls

instance FromField Lemmas where
  parseField = maybe mzero pure . readMaybe . getLenient . convertString

-- CSV records ----------------

data LemmaRecord = LR
  { idText      :: String
  , designation :: String
  , lemmata     :: Lemmas
  } deriving (Eq, Show)

instance FromNamedRecord LemmaRecord where
  parseNamedRecord r = LR <$> r .: "id_text"
                          <*> r .: "Designation"
                          <*> r .: "lemma"

-- Short Stringification ------

class ShortShow a where
  shortShow :: a -> String
instance ShortShow Tag where shortShow = show
instance ShortShow Lemma where
  shortShow l =  lword l
              ++ "[" ++ ltranslation l ++ "]"
              ++ show (ltag l)
instance ShortShow Lemmas where
  shortShow = unwords . map shortShow . lemmas

-- Control Flow ---------------

readRecords :: FilePath -> IO [LemmaRecord]
readRecords = fmap (either fail (V.toList . snd) . decodeByName) . BS.readFile

main :: IO ()
main = do
  records <- readRecords "data/data.csv"
  let lemmi = concatMap (lemmas . lemmata) records
  putStrLn $ show (length records) ++ " records with " ++ show (length lemmi) ++ " found."
  print $ take 3 records
  putStrLn $ unlines . map (unwords . map shortShow . lemmas . lemmata) $ take 3 records
