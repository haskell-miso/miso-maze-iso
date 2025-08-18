{-# LANGUAGE RecordWildCards #-}

module Game.Game where

import Data.Map as M
import Data.Vector qualified as V
import Miso
import Miso.Lens
import Miso.Lens.TH

import Game.Board
import Game.Infos
import Game.Position

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  deriving (Eq)

data Game = Game
  { _gameBoard :: Board
  , _gamePlayers :: Map MisoString Position
  , _gameScore :: Int
  } deriving (Eq)

makeLenses ''Game

playMove :: MisoString -> Move -> Game -> Maybe Game
playMove _ _ g = Just g

getInfos :: MisoString -> Game -> Maybe Infos
getInfos name Game{..} =
  case name `M.lookup` _gamePlayers of
    Nothing -> Nothing
    Just p -> Just $ Infos (elems _gamePlayers) p _gameScore

