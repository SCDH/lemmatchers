module Main where

import Data.Proxy
import Data.Text (Text)
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Servant.API
import Servant
import Servant.HTML.Blaze
import Text.Blaze
import Network.Wai.Handler.Warp
import Web.FormUrlEncoded
import GHC.Generics
import Control.Monad.IO.Class

data MatcherForm = MatcherForm
  { dummy       :: Text
  , mfMatchers  :: Text
  } deriving (Eq, Show, Generic)

instance FromForm MatcherForm

type LemmatchersAPI = FormAPI :<|> MatchingAPI
type FormAPI        = Get '[HTML] Html
type MatchingAPI    = "match" :> ReqBody '[FormUrlEncoded] MatcherForm
                              :> Post '[HTML] Html

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
  defaultMatchers <- liftIO $ readFile "data/matchers.txt"
  return $ template "Lemmatchers" $ do
    H.h1 "Lemmatchers"
    H.form  ! A.action (textValue $ toUrlPiece $ safeLink lemmatchersAPI matchingAPI)
            ! A.method "post"
            $ do
      H.fieldset $ do
        H.legend "Input data"
        H.p "Please provide the input data as a CSV file:"
        H.p $ H.input ! A.type_ "text"
                      ! A.id    "dummy"
                      ! A.name  "dummy"
      H.fieldset $ do
        H.legend "Matcher rules"
        H.textarea  ! A.id    "mfMatchers"
                    ! A.name  "mfMatchers"
                    ! A.rows  "15"
                    ! A.cols  "30"
                    $ H.string defaultMatchers
      H.fieldset $ do
        H.legend "Match!"
        H.p "You will receive a (possibly large) CSV file with matching results."
        H.p $ H.input ! A.type_ "submit"
                      ! A.value "May the matching begin!"

handleMatching :: MatcherForm -> Handler Html
handleMatching mf = undefined

main :: IO ()
main = do
  let port = 4217
  putStrLn $ "Serving Lemmatchers from port " ++ show port ++ "..."
  run port lemmatchers

template :: Html -> Html -> Html
template hTitle hBody = do
    H.docTypeHtml $ do
      H.head $ do
        H.meta ! A.name "viewport" ! A.content "width=device-width,initial-scale=1.0"
        H.title hTitle
      H.body $ do
        hBody
