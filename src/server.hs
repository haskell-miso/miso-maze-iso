{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM (newTVarIO)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipFiles (..), def, gzip, gzipFiles)
-- import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (lookupEnv)

import Server.WsModel
import Server.ServerApp
import Server.WsApp

-- import Network.Protocol
-- import Data.Aeson

main :: IO ()
main = do

  -- print $ encode $ Join "foo bar"
  
  port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
  putStrLn $ "Running on port " <> show port
  wsVar <- newTVarIO =<< mkWsModel "mazes"
  run port
    -- LOG $ logStdout 
    $ wsApp wsVar
    $ compress serverApp
  where
    compress = gzip def{gzipFiles = GzipCompress}

