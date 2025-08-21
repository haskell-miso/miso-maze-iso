{-# LANGUAGE OverloadedStrings #-}

module Server.WsApp (wsApp) where

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, forM_)
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Miso
import Miso.Lens
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

import Game.Common
import Game.Game
import Network.Api
import Network.Protocol
import Server.WsModel

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

recvMsg :: WS.Connection -> IO (Maybe ClientMsg)
recvMsg conn = decode . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: WS.Connection -> ServerMsg -> IO ()
sendMsg conn = WS.sendTextData conn . T.decodeUtf8 . B.toStrict . encode

broadcastPlayers :: TVar WsModel -> IO ()
broadcastPlayers wsVar = do
  (players, conns) <- atomically $ do
    ws <- readTVar wsVar
    let ps = ws ^. wsGame ^. gamePlayers
        cs = ws ^. wsClients & M.elems
    pure (ps, cs)
  T.putStrLn $ "broadcasting to: " <> T.unwords (map _playerName players)
  forM_ conns $ flip sendMsg (UpdatePlayers players)

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
          finally (handleRun wsVar name' conn) (handleClose wsVar name')
    _ -> do
      T.putStrLn "rejected: expecting Join"
      sendMsg conn (Rejected "expecting Join")

handleClose :: TVar WsModel -> MisoString -> IO ()
handleClose wsVar name = do
  atomically $ modifyTVar' wsVar $ \ws -> 
    ws & wsClients %~ M.delete name
       & wsGame %~ deletePlayer name
  broadcastPlayers wsVar
  T.putStrLn $ name <> " left the maze"

handleRun :: TVar WsModel -> MisoString -> WS.Connection -> IO ()
handleRun wsVar name conn = do

  -- get and send the current board
  boardUrl <- atomically (mkStaticUri . _wsCurrentBoard <$> readTVar wsVar)
  T.putStrLn $ "sending: " <> boardUrl
  sendMsg conn (UpdateBoard boardUrl)

  -- get and broadcast the player list
  broadcastPlayers wsVar

  -- handle requests from the client
  forever $ do
    msg <- recvMsg conn
    case msg of
      Just (Play move) -> do
        T.putStrLn $ name <> " plays " <> T.pack (show move)
        pure ()
        -- TODO play move
        -- if new board, broadcast board
        -- broadcast players
      _ -> T.putStrLn "ignored: expecting Play"


-------------------------------------------------------------------------------
-- apps
-------------------------------------------------------------------------------

wsApp :: TVar WsModel -> Application -> Application
wsApp wsVar = websocketsOr WS.defaultConnectionOptions (serverApp wsVar)

serverApp ::TVar WsModel -> WS.PendingConnection -> IO ()
serverApp wsVar pc = do
  conn <- WS.acceptRequest pc
  WS.withPingThread conn 30 (pure ()) (handleConnect wsVar conn)

