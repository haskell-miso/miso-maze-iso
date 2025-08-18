{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Game.Board
import Game.Game
import Game.Infos
import Game.Position

data Model = Model
  { _modelError :: MisoString
  , _modelName :: MisoString
  , _modelInfos :: Maybe Infos
  , _modelBoard :: Maybe Board
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model " " "myname" Nothing Nothing    -- TODO

