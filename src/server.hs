{-# LANGUAGE OverloadedStrings #-}

-- import Control.Concurrent.STM (newTVarIO)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipFiles (..), def, gzip, gzipFiles)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (lookupEnv)

import Server.Model
import Server.ServerApp
import Server.WsApp

main :: IO ()
main = do
  port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
  putStrLn $ "Running on port " <> show port
  run port
    $ logStdout 
    $ wsApp -- TODO modelVar      -- compress?
    $ compress serverApp
  where
    compress = gzip def{gzipFiles = GzipCompress}

