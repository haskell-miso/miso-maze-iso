{-# LANGUAGE RecordWildCards #-}

module Game.Game where

import Data.IntMap qualified as IM
-- import Data.Vector qualified as V
-- import Miso
import Miso.Lens
import Miso.Lens.TH

import Game.Common

data Game = Game
  { _gameBoard :: Board
  , _gamePlayers :: IM.IntMap Player
  , _gameScore :: Int
  } deriving (Eq)

makeLenses ''Game

playMove :: Int -> Move -> Game -> Maybe Game
playMove _ _ g = Just g

getInfos :: Int -> Game -> Maybe Infos
getInfos col Game{..} =
  case col `IM.lookup` _gamePlayers of
    Nothing -> Nothing
    Just _ -> Just $ Infos players _gameScore
  where
    players = []  -- TODO

