{-# LANGUAGE OverloadedStrings #-}

module Lemmatchers.Matchers where

import Lemmatchers.TagRecords
import Data.Bool
import Data.List
import Data.String.Encode
import Data.Bifunctor
import qualified Text.Read as R
import Text.ParserCombinators.ReadP
import Control.Applicative (Alternative, liftA2, empty)
import Control.Monad
import Data.Csv

lb, rn, line, sep :: String
lb    = "\n" -- "\r\n" would work, too
rn    = "\r\n"
line  = "---"
sep   = lb ++ line ++ lb ++ lb

plb, pline, psep :: ReadP ()
plb   = void $ optional (char '\r') >> char '\n'
pline = void $ string line
psep  = between (plb >> many1 plb) (plb >> many1 plb) pline

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded = liftA2 (bool empty) pure

-------------------------------

newtype Matchers = Matchers
  { matchers :: [Matcher]
  } deriving Eq

instance Show Matchers where
  show = intercalate sep . map show . matchers

instance Read Matchers where
  readsPrec _ = readP_to_S parseMatchers

parseMatchers :: ReadP Matchers
parseMatchers = Matchers <$> parseMatcher `sepBy1` psep

-------------------------------

data Matcher = Matcher
  { name      :: String
  , patterns  :: [Pattern]
  } deriving Eq

instance Show Matcher where
  show (Matcher n ps) = unlines $ ("matcher " ++ n) : map show ps

instance Read Matcher where
  readsPrec _ = readP_to_S parseMatcher

parseMatcher :: ReadP Matcher
parseMatcher = do
    n   <- string "matcher " >> munch1 (`notElem` rn)
    ps  <- plb >> patternParser `sepBy1` plb
    return $ Matcher n ps

-------------------------------

newtype Pattern = Pattern
  { items :: [MatchItem]
  } deriving Eq

instance Show Pattern where
  show = unwords . map show . items

instance Read Pattern where
  readsPrec _ = readP_to_S patternParser

patternParser :: ReadP Pattern
patternParser = Pattern <$> matchItemParser `sepBy1` char ' '

-------------------------------

data MatchItem
  = Exact   Tag
  | Not     Tag
  | OneOf   [Tag]
  | NoneOf  [Tag]
  | Any
  deriving Eq

instance Show MatchItem where
  show  (Exact  e)  = show e
  show  (Not    n)  = "-" ++ show n
  show  (OneOf  ts) = "(" ++ unwords (show <$> ts) ++ ")"
  show  (NoneOf ts) = "-" ++ show (OneOf ts)
  show  Any         = "*"

instance Read MatchItem where
  readsPrec _ = readP_to_S matchItemParser

matchItemParser :: ReadP MatchItem
matchItemParser = choice [exact, notmi, oneof, noneof, anymi]
  where exact     =             Exact   <$> tag
        notmi     = char '-' >> Not     <$> tag
        oneof     =             OneOf   <$> tags
        noneof    = char '-' >> NoneOf  <$> tags
        anymi     = char '*' >> return Any
        tags      = between (char '(') (char ')') $ tag `sepBy1` char ' '
        tag       = maybe pfail return . R.readMaybe =<< ucToken
        ucToken   = munch1 (`elem` ['A'..'Z'])

-------------------------------

data Match = Match
  { matcher           :: Matcher
  , pattern           :: Pattern
  , foundIdText       :: String
  , foundDesignation  :: String
  , foundLemmata      :: [Lemma]
  } deriving Eq

instance Show Match where
  show (Match m p fid fd fl) = intercalate " ; "
    [name m, show p, fid, fd, shortShow (Lemmas fl)]

instance ToNamedRecord Match where
  toNamedRecord (Match m p fid fd fl) = namedRecord $
    map (second convertString)
      [ ("matcher",     name m)
      , ("pattern",     show p)
      , ("id_text",     fid)
      , ("Designation", fd)
      , ("lemma",       shortShow (Lemmas fl))
      ]

-------------------------------

matchRecord :: Matcher -> LemmaRecord -> [Match]
matchRecord m lr  = concatMap collect (patterns m)
  where ls        = lemmas (lemmata lr)
        collect p = map (build p) $ matchLemmas (items p) ls
        build   p = Match m p (idText lr) (designation lr)

matchLemmas :: [MatchItem] -> [Lemma] -> [[Lemma]]
matchLemmas _ []           = []
matchLemmas mis lss@(_:ls)
  | length mis > length lss = []
  | otherwise               =
    case matchPrefix (map ((. ltag) . matchTag) mis) lss of
      Nothing -> matchLemmas mis ls
      Just m  ->  let rest = drop (length mis) lss
                  in  m : matchLemmas mis rest

matchPrefix :: [a -> Bool] -> [a] -> Maybe [a]
matchPrefix [] _ = Just []
matchPrefix _ [] = Nothing
matchPrefix (p:ps) (x:xs)
  | p x       = (x:) <$> matchPrefix ps xs
  | otherwise = Nothing

matchTag :: MatchItem -> Tag -> Bool
matchTag (Exact   e)  = (== e)
matchTag (Not     n)  = (/= n)
matchTag (OneOf   ts) = (`elem` ts)
matchTag (NoneOf  ts) = (`notElem` ts)
matchTag Any          = const True
