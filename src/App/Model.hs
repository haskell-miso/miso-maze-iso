{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH
import Miso.WebSocket

import Game.Common

data Model = Model
  { _modelError :: MisoString
  , _modelWsUrl :: MisoString
  , _modelWs :: WebSocket
  , _modelInputName :: MisoString
  , _modelPlayers :: [Player]
  , _modelBoard :: Maybe Board
  } deriving (Eq)

makeLenses ''Model

mkModel :: MisoString -> Model
mkModel wsUrl = Model 
  { _modelError = " "
  , _modelWsUrl = wsUrl
  , _modelWs = emptyWebSocket
  , _modelInputName = " "
  , _modelPlayers = []
  , _modelBoard = Nothing
  }

