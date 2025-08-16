{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Maybe (fromMaybe)
import Miso hiding (run)
import Network.HTTP.Types
import Network.Wai (responseLBS)
import Network.Wai.Application.Static (defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipFiles (..), def, gzip, gzipFiles)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import System.Environment (lookupEnv)

import App.Component 
import App.Model
import Domain.Hero
import Server.Api

-------------------------------------------------------------------------------
-- server data
-------------------------------------------------------------------------------

heroes :: [Hero]
heroes = 
    [ Hero "Scooby Doo" "scoobydoo.png"
    , Hero "Sponge Bob" "spongebob.png"
    ]

-------------------------------------------------------------------------------
-- server routing
-------------------------------------------------------------------------------

type RouteHome a = a
type Routes a = RouteHome a
type ClientRoutes = Routes (Get '[HTML] Page)

type ServerApi
  =    PublicApi
  :<|> ClientRoutes
  :<|> Raw

newtype Page = Page AppComponent

handlePublicApi :: Server PublicApi
handlePublicApi 
  =    serveDirectoryWith (defaultWebAppSettings "public")
  :<|> pure heroes

handleClientRoutes :: Server ClientRoutes
handleClientRoutes 
  = pure (Page appComponent)

handle404 :: Application
handle404 _ respond' =
  respond' $
    responseLBS status404 [("Content-Type", "text/html")] $
      toHtml [ p_ [] [ "page not found" ] ]

-------------------------------------------------------------------------------
-- server rendering
-------------------------------------------------------------------------------

instance ToHtml Page where
  toHtml (Page x) =
    toHtml
      [ doctype_
      , html_
        [ lang_ "en" ]
        [ head_ 
          [ title_ "miso-maze" ]
          [ meta_ [ charset_ "utf-8" ]
          , meta_ [ name_ "viewport" , content_ "width=device-width, initial-scale=1" ]
          , link_
            [ rel_ "icon"
            , href_ (mkStaticUri "favicon.ico")
            , type_ "image/x-icon"
            ]
          , script_ [ src_ (mkStaticUri "index.js"), type_ "module" ] ""
          , body_ [] [ toView @Model x] ]
        ]
      ]

-------------------------------------------------------------------------------
-- run server app
-------------------------------------------------------------------------------

main :: IO ()
main = do
  print $ mkStaticUri "favicon.ico"
  port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
  putStrLn $ "Running on port " <> show port <> "..."
  run port $ logStdout $ compress serverApp
  where
    compress = gzip def{gzipFiles = GzipCompress}

serverApp :: Application
serverApp = serve (Proxy @ServerApi) serverHandlers
  where
    serverHandlers 
      =    handlePublicApi
      :<|> handleClientRoutes
      :<|> Tagged handle404

