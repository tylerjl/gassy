module Main where

import ClassyPrelude hiding (for)

import Data.Aeson
import qualified Data.Vector as V
import Database.V5.Bloodhound hiding (name)
import Network.HTTP.Client (defaultManagerSettings)
import Network.Wai.Middleware.Static
import Web.Spock
import Web.Spock.Config

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title)

import qualified Text.Blaze.Html.Renderer.Text as R

application :: H.Html
application = "Gassy"

es :: Server
es = Server "http://elasticsearch.service.consul:9200"

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8090 (spock spockCfg app)

app :: SpockM () () () ()
app = do
  middleware (staticPolicy (addBase "static"))
  get root homePage
  post root $ do
    date' <- param "date"
    cost' <- param "cost"
    case (date', cost') of
      (Just date, Just cost) -> do
        _ <- liftIO $ indexDoc date cost
        pure ()
      _ -> pure ()
    homePage
  where homePage = html . toStrict $ R.renderHtml home

indexDoc :: Text -> Text -> IO Reply
indexDoc timestamp cost = do
  withBH defaultManagerSettings es $
    bulk $ V.fromList
      [ BulkIndexAuto (IndexName "gas") (MappingName "_doc") $
        object [ "timestamp" .= timestamp
               , "price" .= cost
               ]
      ]

home :: H.Html
home = H.docTypeHtml $ do
  H.head $ do
    H.title application
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/style.css"
  H.body $ do
    H.h1 application
    H.form ! action "/" ! method "POST" $ do
      H.label "Purchase Date" ! for "date"
      H.input ! type_ "date" ! name "date" ! Text.Blaze.Html5.Attributes.id "date"

      H.label "Cost" ! for "cost"
      H.input ! type_ "number" ! name "cost"
        ! Text.Blaze.Html5.Attributes.id "cost"
        ! step "0.01"

      H.input ! type_ "submit" ! value "Save"
