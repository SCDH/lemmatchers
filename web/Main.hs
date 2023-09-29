module Main where

import Lemmatchers.Matchers
import Lemmatchers.TagRecords
import qualified Data.Text as T
import Data.Either.Extra
import Data.Csv
import qualified Data.Vector as V
import Data.Proxy
import Text.Read
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Servant.API
import Servant
import Servant.HTML.Blaze
import Servant.Multipart
import Servant.CSV.Cassava
import Text.Blaze
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class

data MatcherFormData = MFD
  { mfMatchers      :: Matchers
  , mfLemmaRecords  :: [LemmaRecord]
  } deriving Show

instance FromMultipart Mem MatcherFormData where
  fromMultipart mpd = do
    msS <- T.unpack <$> lookupInput "matchers" mpd
    ms  <- maybeToEither ("Unable to parse matchers: " ++ msS) $ readMaybe msS
    csv <- fdPayload <$> lookupFile "lemmasCSV" mpd
    lrs <- V.toList . snd <$> decodeByName csv
    return $ MFD ms lrs

type LemmatchersAPI = FormAPI :<|> MatchingAPI
type FormAPI        = Get '[HTML] Html
type MatchingAPI    = "match" :> MultipartForm Mem MatcherFormData
                              :> Post '[CSV] [Match]

lemmatchersAPI  :: Proxy LemmatchersAPI
formAPI         :: Proxy FormAPI
matchingAPI     :: Proxy MatchingAPI
lemmatchersAPI  = Proxy
formAPI         = Proxy
matchingAPI     = Proxy

lemmatchers :: Application
lemmatchers = serve lemmatchersAPI  $   handleForm
                                  :<|>  handleMatching

handleForm :: Handler Html
handleForm = do
  let formURL = toUrlPiece $ safeLink lemmatchersAPI matchingAPI
  defaultMatchers <- liftIO $ readFile "data/matchers.txt"
  return $ do
    H.docTypeHtml $ do
      H.head $ do
        H.meta ! A.name "viewport" ! A.content "width=device-width,initial-scale=1.0"
        H.title "Lemmatchers"
      H.body $ do
        H.h1 "Lemmatchers"
        H.form  ! A.action (textValue formURL)
                ! A.method "post"
                ! A.enctype "multipart/form-data"
                $ do
          H.fieldset $ do
            H.legend "Input data"
            H.p "Please provide the input data as a CSV file:"
            H.p $ H.input ! A.type_ "file"
                          ! A.name  "lemmasCSV"
          H.fieldset $ do
            H.legend "Matcher rules"
            H.textarea  ! A.name  "matchers"
                        ! A.rows  "15"
                        ! A.cols  "30"
                        $ H.string defaultMatchers
          H.fieldset $ do
            H.legend "Match!"
            H.p "You will receive a (possibly large) CSV file with matching results."
            H.p $ H.input ! A.type_ "submit"
                          ! A.value "May the matching begin!"

handleMatching :: MatcherFormData -> Handler [Match]
handleMatching (MFD ms lrs) =
  return $ concat [matchRecord m lr | m <- matchers ms, lr <- lrs]

main :: IO ()
main = do
  let port = 4217
  putStrLn $ "Serving Lemmatchers from port " ++ show port ++ "..."
  run port lemmatchers
