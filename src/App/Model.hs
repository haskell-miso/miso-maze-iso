{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Domain.Game

data Model = Model
  { _modelError :: MisoString
  , _modelName :: MisoString
  , _modelInfos :: Infos
  , _modelBoard :: Board
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model " " "myname" mkInfos mkBoard

