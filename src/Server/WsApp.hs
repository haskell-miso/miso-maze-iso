{-# LANGUAGE OverloadedStrings #-}

module Server.WsApp (wsApp) where

import Data.Aeson
import Data.Text.IO qualified as T
import Control.Concurrent.STM
import Control.Monad (forever)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

import Network.Protocol
import Server.WsModel

-------------------------------------------------------------------------------
-- ws app
-------------------------------------------------------------------------------

wsApp :: TVar WsModel -> Application -> Application
wsApp _wsVar = websocketsOr WS.defaultConnectionOptions serverApp

serverApp ::WS.PendingConnection -> IO ()
serverApp pc = do
  conn <- WS.acceptRequest pc
  WS.withPingThread conn 30 (pure ()) (runHandler conn)    -- TODO finally

runHandler :: WS.Connection -> IO ()
runHandler conn = forever $ do
    msg <- recvMsg conn
    -- msg <- WS.receiveDataMessage conn
    print msg
    case msg of
      Just (Join name) -> T.putStrLn $ name <> " has joined the maze"
      _ -> putStrLn "TODO"
    -- TODO receiveData receiveDataMessage

-------------------------------------------------------------------------------
-- basic helpers
-------------------------------------------------------------------------------


recvMsg :: WS.Connection -> IO (Maybe ClientMsg)
recvMsg conn = do
  d <- WS.receiveData conn
  print d
  let s = WS.fromLazyByteString d
  print s
  let r = decode s
  print r
  pure r
-- recvMsg conn = decode . WS.fromLazyByteString <$> WS.receiveData conn

{-
sendMsg :: MsgToClient -> WS.Connection -> IO ()
sendMsg msg conn = WS.sendTextData conn (fmtMsgToClient msg)

-}

