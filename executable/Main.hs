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
    H.meta ! charset "utf-8"
    H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title application
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/style.css"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/bulma.min.css"
  H.body $ do
    H.section ! class_ "section" $ do
      H.div ! class_ "container" $ do
        H.h1 ! class_ "title" $ application
        H.form ! action "/" ! method "POST" $ do
          H.div ! class_ "field" $ do
            H.label "Purchase Date" ! class_ "label" ! for "date"
            H.div ! class_ "control" $ do
              H.input ! class_ "input" ! type_ "date" ! name "date" ! Text.Blaze.Html5.Attributes.id "date"

          H.div ! class_ "field" $ do
            H.label "Cost" ! class_ "label" ! for "cost"
            H.div ! class_ "control" $ do
              H.input ! type_ "number" ! class_ "input" ! name "cost"
                ! Text.Blaze.Html5.Attributes.id "cost"
                ! step "0.01"

          H.div ! class_ "field" $ do
            H.label "Odometer" ! class_ "label" ! for "miles"
            H.div ! class_ "control" $ do
              H.input ! type_ "number" ! class_ "input" ! name "miles"
                ! Text.Blaze.Html5.Attributes.id "miles"

          H.div ! class_ "field" $ do
            H.div ! class_ "control" $ do
              H.button ! class_ "button is-link" $ "Save"
