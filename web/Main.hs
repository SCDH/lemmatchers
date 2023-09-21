module Main where

import Data.Proxy
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import Servant.API
import Servant
import Servant.HTML.Blaze
import Network.Wai.Handler.Warp

type LemmatchersAPI = Get '[HTML] Html

handleHelloWorld :: Handler Html
handleHelloWorld = return $ do
  H.docTypeHtml $ do
    H.head $ do
      H.title "Lemmatchers"
    H.body $ do
      H.p "Hello, world!"

main :: IO ()
main = do
  let port = 4217
  putStrLn $ "Serving Lemmatchers from port " ++ show port ++ "..."
  run port $ serve (Proxy :: Proxy LemmatchersAPI)
    $ handleHelloWorld
