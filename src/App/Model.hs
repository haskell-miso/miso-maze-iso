{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Game.Common

-- TODO board name?

data Model = Model
  { _modelError :: MisoString
  , _modelInputName :: MisoString
  , _modelPlayers :: [Player]
  , _modelBoard :: Maybe Board
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model  " " " " [] Nothing

