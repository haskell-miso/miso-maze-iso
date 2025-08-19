{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Game.Common

-- TODO board name?

data Model = Model
  { _modelWsUrl :: MisoString
  , _modelError :: MisoString
  , _modelInputName :: MisoString
  , _modelPlayers :: [Player]
  , _modelBoard :: Maybe Board
  } deriving (Eq)

makeLenses ''Model

mkModel :: MisoString -> Model
mkModel wsUrl = Model wsUrl " " " " [] Nothing

