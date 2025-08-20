{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server.WsModel where

import Data.List (sort)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Miso
import Miso.Lens
import Miso.Lens.TH
import Network.WebSockets qualified as WS
import System.Directory (listDirectory)

import Game.Game

data WsModel = WsModel
  { _wsGame :: Game
  , _wsClients :: M.Map T.Text WS.Connection
  , _wsCurrentBoard :: T.Text
  , _wsBoardFiles :: [FilePath]
  }

makeLenses ''WsModel

mkWsModel :: FilePath -> IO WsModel
mkWsModel dir = do
  mazeFiles <- map (dir++) . sort <$> listDirectory dir
  putStrLn $ "maze files: " <> unwords mazeFiles
  pure $ WsModel mkGame M.empty " " mazeFiles

nextBoard :: WsModel -> IO WsModel
nextBoard ws@WsModel{..} = do 
  case _wsBoardFiles of
    [] -> pure ws
    (f:fs) -> do
      str <- T.readFile f
      case loadBoard str _wsGame of
        Nothing -> pure ws
        Just game -> pure $ ws & wsBoardFiles .~ fs ++ [f]
                               & wsCurrentBoard .~ ms f
                               & wsGame .~ game

addClient :: MisoString -> WS.Connection -> WsModel -> Either MisoString WsModel
addClient name conn ws =
  case tryAddPlayer name (ws^.wsGame) of
    Left err -> Left err
    Right game -> Right $ ws & wsClients %~ M.insert name conn
                             & wsGame .~ game

