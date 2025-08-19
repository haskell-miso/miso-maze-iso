
module Server.WsApp (wsApp) where

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
    msg <- WS.receiveDataMessage conn
    -- TODO receiveData receiveDataMessage
    print msg

-------------------------------------------------------------------------------
-- basic helpers
-------------------------------------------------------------------------------

{-

recvMsg :: WS.Connection -> IO (Maybe ClientMsg)
recvMsg conn = parseMsgToServer . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: MsgToClient -> WS.Connection -> IO ()
sendMsg msg conn = WS.sendTextData conn (fmtMsgToClient msg)

-}

