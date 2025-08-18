
module Server.WsApp (wsApp) where

import Control.Concurrent.STM
import Control.Monad (forever)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

import Network.Protocol
import Server.Model

wsApp :: TVar Model -> Application -> Application
wsApp modelVar = websocketsOr WS.defaultConnectionOptions (serverApp modelVar)

serverApp ::TVar Model -> WS.PendingConnection -> IO ()
serverApp modelVar pc = do
  conn <- WS.acceptRequest pc
  WS.withPingThread conn 30 (pure ()) (runHandler modelVar conn)    -- TODO finally

runHandler :: TVar Model -> WS.Connection -> IO ()
runHandler modelVar conn = forever $ do
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

