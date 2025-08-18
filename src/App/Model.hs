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
  , _modelInfos :: Infos
  , _modelBoard :: Board
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model " " "myname" infos board    -- TODO
  where
    infos = mkInfos [] (Position 1 1) 0
    board = mkBoard

