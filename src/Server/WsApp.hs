{-# LANGUAGE OverloadedStrings #-}

module Server.WsApp (wsApp) where

import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Control.Concurrent.STM
import Control.Monad (forever)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

import Game.Common
import Network.Protocol
import Server.WsModel

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

recvMsg :: WS.Connection -> IO (Maybe ClientMsg)
recvMsg conn = decode . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: WS.Connection -> ServerMsg -> IO ()
sendMsg conn = WS.sendTextData conn . T.decodeUtf8 . B.toStrict . encode

-------------------------------------------------------------------------------
-- ws app
-------------------------------------------------------------------------------

wsApp :: TVar WsModel -> Application -> Application
wsApp _wsVar = websocketsOr WS.defaultConnectionOptions serverApp

serverApp ::WS.PendingConnection -> IO ()
serverApp pc = do
  conn <- WS.acceptRequest pc
  WS.withPingThread conn 30 (pure ()) (runHandler conn)
  -- TODO register the client
  -- TODO finally remove the client

runHandler :: WS.Connection -> IO ()
runHandler conn = forever $ do
    msg <- recvMsg conn
    case msg of
      Just (Join name) -> do
        T.putStrLn $ name <> " has joined the maze"
        sendMsg conn (UpdatePlayers [Player "foo" 16711680 (Position 1 2)])
        -- TODO UpdatePlayers
        sendMsg conn (UpdateBoard "mazes/maze1.json")
        -- TODO UpdateBoard
      _ -> putStrLn "TODO"


