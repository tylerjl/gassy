module Main where

import ClassyPrelude hiding (for, min)

import Control.Lens hiding ((.=))
import Control.Logger.Simple
import Data.Aeson
import Data.Aeson.Lens
import Data.Time
import qualified Data.Vector as V
import Database.V5.Bloodhound hiding (key, name)
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
  withGlobalLogging (LogConfig Nothing True) $ do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8090 (spock spockCfg app)

app :: SpockM () () () ()
app = do
  middleware (staticPolicy (addBase "static"))
  get root $
    liftIO today >>= homePage Nothing
  post root $ do
    date' <- param "date"
    cost' <- param "cost"
    esResponse <- case (date', cost') of
      (Just date, Just cost) -> do
        reply <- liftIO $ indexDoc date cost
        resp <- liftIO (parseEsResponse reply)
        return $ Just resp
      _ -> return Nothing
    liftIO today >>= homePage esResponse
  where homePage r t = html . toStrict $ R.renderHtml $ home r t
        today = do
          t <- getZonedTime
          return $ formatTime defaultTimeLocale "%Y-%m-%d" t

indexDoc :: Text -> Text -> IO Reply
indexDoc timestamp cost = do
  withBH defaultManagerSettings es $
    bulk $ V.fromList
      [ BulkIndexAuto (IndexName "gas") (MappingName "_doc") $
        object [ "timestamp" .= timestamp
               , "price" .= cost
               ]
      ]

home :: Maybe (Either EsError Value) -> String -> H.Html
home flash timestamp = H.docTypeHtml $ do
  H.head $ do
    H.meta ! charset "utf-8"
    H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title application
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/style.css"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/bulma.min.css"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/animate.min.css"
  H.body $ do
    H.section ! class_ "section" $ do
      H.div ! class_ "container" $ do
        notification flash
        H.h1 ! class_ "title" $ application
        H.form ! action "/" ! method "POST" $ do

          H.div ! class_ "field" $ do
            H.label "Purchase Date" ! class_ "label" ! for "date"
            H.div ! class_ "control" $ do
              H.input ! class_ "input" ! type_ "date" ! name "date"
                ! Text.Blaze.Html5.Attributes.id "date"
                ! value (H.stringValue timestamp)

          H.div ! class_ "field" $ do
            H.label "Cost" ! class_ "label" ! for "cost"
            H.div ! class_ "control" $ do
              H.input ! type_ "number" ! min "0" ! class_ "input" ! name "cost"
                ! Text.Blaze.Html5.Attributes.id "cost"
                ! step "0.01"

          H.div ! class_ "field" $ do
            H.label "Gallons" ! class_ "label" ! for "gallons"
            H.div ! class_ "control has-icon-left" $ do
              H.input ! type_ "number" ! min "0" ! class_ "input" ! name "gallons"
                ! Text.Blaze.Html5.Attributes.id "gallons"
                ! step "0.001"

          H.div ! class_ "field" $ do
            H.label "Odometer" ! class_ "label" ! for "miles"
            H.div ! class_ "control" $ do
              H.input ! type_ "number" ! min "0" ! class_ "input" ! name "miles"
                ! Text.Blaze.Html5.Attributes.id "miles"

          H.div ! class_ "field" $ do
            H.div ! class_ "control" $ do
              H.button ! class_ "button is-link" $ "Save"

  where notification (Just (Right val)) =
          let
            (sev, msg :: Text) =
              if val ^? key "errors" . _Bool == Just True then
                ( "warning"
                , "Problem storing data: "
                  <> intercalate ", " (val ^.. deep (key "reason" . _String))
                  <> "."
                )
              else
                ( "primary"
                , "Logged mileage successfully ("
                  <> intercalate ", " (val ^.. deep (key "_id" . _String))
                  <> ")."
                )
          in
            H.div ! class_ ("notification animated flipOutX delay-2s is-" <> sev) $ H.toHtml msg
        notification _ = mempty
