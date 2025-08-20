{-# LANGUAGE OverloadedStrings #-}

module Server.WsApp (wsApp) where

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Miso
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

-- broadcastPlayers :: TVar WsModel -> IO ()
-- broadcastPlayers wsVar = do
  

-------------------------------------------------------------------------------
-- handlers
-------------------------------------------------------------------------------

handleConnect :: TVar WsModel -> WS.Connection -> IO ()
handleConnect wsVar conn = do
  msg <- recvMsg conn
  case msg of
    Just (Join name) -> do
      res <- atomically $ tryAddClient name conn wsVar
      case res of
        Left err -> do
          T.putStrLn $ "rejected: " <> err
          sendMsg conn (Rejected err)
        Right name' -> do
          T.putStrLn $ name' <> " has joined the maze"
          finally (handleRun wsVar conn) (handleClose wsVar name')
          handleRun wsVar conn
    _ -> do
      T.putStrLn "rejected: expected Join"
      sendMsg conn (Rejected "expected Join")

handleClose :: TVar WsModel -> MisoString -> IO ()
handleClose wsVar name = do
  -- TODO remove client inside model
  T.putStrLn $ name <> " left the maze"
  pure ()

handleRun :: TVar WsModel -> WS.Connection -> IO ()
handleRun wsVar conn = do
  -- TODO send board to the new client
  -- TODO broadcast players
  sendMsg conn (UpdatePlayers [Player "foo" 16711680 (Position 1 2)])

  boardUrl <- atomically (getBoardUrl <$> readTVar wsVar)
  T.putStrLn $ "sending: " <> boardUrl
  sendMsg conn (UpdateBoard boardUrl)

  forever $ do
    msg <- recvMsg conn
    case msg of
      Just (Play move) -> do
        print move
        pure ()
        -- TODO play move
        -- if new board, broadcast board
        -- broadcast players
      _ -> T.putStrLn "ignored: expected Play"


-------------------------------------------------------------------------------
-- apps
-------------------------------------------------------------------------------

wsApp :: TVar WsModel -> Application -> Application
wsApp wsVar = websocketsOr WS.defaultConnectionOptions (serverApp wsVar)

serverApp ::TVar WsModel -> WS.PendingConnection -> IO ()
serverApp wsVar pc = do
  conn <- WS.acceptRequest pc
  WS.withPingThread conn 30 (pure ()) (handleConnect wsVar conn)

