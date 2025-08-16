{-# LANGUAGE OverloadedStrings #-}

module App.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Domain.Hero (Hero)
import Domain.Game

data Model = Model
  { _modelHeroes :: [Hero]
  , _modelError :: MisoString
  -- , _modelInfos :: Infos
  -- , _modelName :: MisoString
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model [] " "    -- warning: an empty would cause hydration to fail

