{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Game.Common

data Model = Model
  { _modelError :: MisoString
  , _modelInputName :: MisoString
  , _modelInfos :: Maybe Infos
  , _modelBoard :: Maybe Board
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model " " " " Nothing Nothing

