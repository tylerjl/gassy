module Main where

import Control.Lens
import Data.Aeson.Lens
import Data.List
import Data.Time
import Database.V5.Bloodhound hiding (key, name)
import Magicbane hiding (Server)
import Network.HTTP.Client (defaultManagerSettings)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import RIO hiding (for)
import Servant.HTML.Blaze
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes hiding (title)
import Web.FormUrlEncoded (FromForm)

import qualified Data.Vector as V
import qualified Text.Blaze.Html5 as H

data FillUp = FillUp
  { date     :: Day
  , gallons  :: Float
  , odometer :: Int
  , sale     :: Float
  } deriving (Eq, Generic, Show)

instance FromForm FillUp
instance FromJSON FillUp
instance ToJSON FillUp

type API = Get '[HTML] H.Html
      :<|> ReqBody '[FormUrlEncoded] FillUp :> Post '[HTML] H.Html

routes :: ServerT API App
routes = homePage mempty
    :<|> addFillUp

api :: Proxy API
api = Proxy

data AppConf = AppConf { elasticsearchHost :: Text } deriving (Generic, Show)

instance DefConfig AppConf where
  defConfig = AppConf { elasticsearchHost = "http://localhost:9200" }

instance FromEnv AppConf

type AppContext = (AppConf)
type App = RIO AppContext

main :: IO ()
main = withEnvConfig $ \conf -> do
  defWaiMain $
    logStdout $
    staticPolicy (addBase "static") $
    magicbaneApp api EmptyContext (conf) routes

application :: H.Html
application = "Gassy"

indexDoc :: (MonadIO m, Has AppConf α, MonadReader α m)
         => Value -> m Reply
indexDoc doc = do
  manager <- liftIO $ newManager defaultManagerSettings
  esHost <- askOpt elasticsearchHost
  runBH (es esHost manager) $ bulk $ V.fromList $
    [ BulkIndexAuto (IndexName "gas") (MappingName "_doc") doc ]
  where es h m = mkBHEnv (Server h) m

today :: IO String
today = do
  getZonedTime >>= return . formatTime defaultTimeLocale "%Y-%m-%d"

addFillUp :: FillUp -> App H.Html
addFillUp fillUp = do
  r' <- indexDoc $ toJSON fillUp
  resp <- liftIO $ (parseEsResponse r' :: IO (Either EsError Value))
  homePage $ notification resp

homePage :: H.Html -> App H.Html
homePage flash = do
  timestamp <- liftIO $ today
  return $ H.docTypeHtml $ do
    H.head $ do
      H.meta ! charset "utf-8"
      H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
      H.title application
      H.link ! rel "stylesheet" ! type_ "text/css" ! href_ "/style.css"
      H.link ! rel "stylesheet" ! type_ "text/css" ! href_ "/bulma.min.css"
      H.link ! rel "stylesheet" ! type_ "text/css" ! href_ "/animate.min.css"
    H.body $ do
      H.section ! class_ "section" $ do
        H.div ! class_ "container" $ do
          flash
          H.h1 ! class_ "title" $ application
          H.form ! action "/" ! method_ "POST" $ do

            H.div ! class_ "field" $ do
              H.label "Purchase Date" ! class_ "label" ! for "date"
              H.div ! class_ "control" $ do
                H.input ! class_ "input" ! type_ "date" ! name "date"
                  ! id_ "date"
                  ! value (H.stringValue timestamp)

            H.div ! class_ "field" $ do
              H.label "Cost" ! class_ "label" ! for "sale"
              H.div ! class_ "control" $ do
                H.input ! type_ "number" ! min_ "0" ! class_ "input" ! name "sale"
                  ! id_ "sale"
                  ! step "0.01"

            H.div ! class_ "field" $ do
              H.label "Gallons" ! class_ "label" ! for "gallons"
              H.div ! class_ "control has-icon-left" $ do
                H.input ! type_ "number" ! min_ "0" ! class_ "input" ! name "gallons"
                  ! id_ "gallons"
                  ! step "0.001"

            H.div ! class_ "field" $ do
              H.label "Odometer" ! class_ "label" ! for "odometer"
              H.div ! class_ "control" $ do
                H.input ! type_ "number" ! min_ "0" ! class_ "input" ! name "odometer"
                  ! id_ "odometer"

            H.div ! class_ "field" $ do
              H.div ! class_ "control" $ do
                H.button ! class_ "button is-link" $ "Save"

notification :: Either a Value -> H.Html
notification (Right val) =
  let
    details key' = intercalate ", " $ map show $ val ^.. deep (key key' . _String)
    (sev, msg) =
      if val ^? key "errors" . _Bool == Just True then
        ( "warning" , "Problem storing data: " <> details "reason" <> "." )
      else
        ( "primary" , "Logged mileage successfully (" <> details "_id" <> ")." )
  in
    H.div ! class_ ("notification animated slideOutUp delay-2s is-" <> sev) $ H.toHtml msg
notification _ = mempty

-- Utils

href_ :: H.AttributeValue -> H.Attribute
href_ = Text.Blaze.Html5.Attributes.href

id_ :: H.AttributeValue -> H.Attribute
id_ = Text.Blaze.Html5.Attributes.id

method_ :: H.AttributeValue -> H.Attribute
method_ = Text.Blaze.Html5.Attributes.method

min_ :: H.AttributeValue -> H.Attribute
min_ = Text.Blaze.Html5.Attributes.min
