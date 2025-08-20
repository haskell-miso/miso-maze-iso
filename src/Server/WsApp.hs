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
-- handlers
-------------------------------------------------------------------------------

handleConnect :: TVar WsModel -> WS.Connection -> IO ()
handleConnect wsVar conn = do
  msg <- recvMsg conn
  case msg of
    Just (Join name) -> do
      T.putStrLn $ name <> " has joined the maze"
      -- TODO register client in wsApp
      -- TODO send board to the new client
      -- TODO broadcast players
      handleRun wsVar conn
      -- TODO finally remove the client
    _ -> do
      T.putStrLn "rejected: expected Join"
      sendMsg conn (Rejected "expected Join")

handleRun :: TVar WsModel -> WS.Connection -> IO ()
handleRun wsVar conn = forever $ do
  msg <- recvMsg conn
  case msg of
    Just (Play move) -> do
      print move
      pure ()
      -- TODO play move
      -- if new board, broadcast board
      -- broadcast players
    _ -> T.putStrLn "ignored: expected Play"

{-
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
-}

-------------------------------------------------------------------------------
-- apps
-------------------------------------------------------------------------------

wsApp :: TVar WsModel -> Application -> Application
wsApp wsVar = websocketsOr WS.defaultConnectionOptions (serverApp wsVar)

serverApp ::TVar WsModel -> WS.PendingConnection -> IO ()
serverApp wsVar pc = do
  conn <- WS.acceptRequest pc
  WS.withPingThread conn 30 (pure ()) (handleConnect wsVar conn)

