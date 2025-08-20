{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Game where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Game.Common

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

playerColors :: [Int]
playerColors =
  [ 0xFF0000
  , 0xFFFF00
  -- , 0xFFFFFF
  -- , 0xFF00FF
  ]

nbMeetings :: Int
nbMeetings = 10

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Game = Game
  { _gameBoard :: Maybe Board
  , _gamePlayers :: [Player]
  , _gameFreeColors :: [Int]
  , _gameRemMeetings :: Int
  } deriving (Eq)

makeLenses ''Game

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

mkGame :: Game
mkGame = Game Nothing [] playerColors 0

tryAddPlayer :: MisoString -> Game -> Either MisoString Game
tryAddPlayer name g@Game{..} =
  case _gameFreeColors of
    [] -> Left "the maze is full"
    (c:cs) -> 
      if any ((==name) . _playerName) _gamePlayers
        then Left $ name <> " is already in the maze"
        else Right $ g & gameFreeColors .~ cs
                       & gamePlayers %~ (Player name c pos :)
  where
    pos = Position 1 1  -- TODO random

loadBoard :: MisoString -> Game -> Maybe Game
loadBoard str g =
  case parseBoard str of
    Nothing -> Nothing
    Just b -> Just $ g & gameBoard ?~ b
                       & gameRemMeetings .~ nbMeetings


playMove :: Int -> Move -> Game -> Maybe Game
playMove _ _ g = Just g

{-

getPlayers :: Int -> Game -> [Player]
getPlayers col Game{..} =
  case col `IM.lookup` _gamePlayers of
    Nothing -> []
    Just _ -> Infos players _gameScore
  where
    players = []  -- TODO
-}

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------



