{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Game where

import Miso hiding (go)
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
  , 0xFFFFFF
  , 0xFF00FF
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

deletePlayer :: MisoString -> Game -> Game
deletePlayer name g@Game{..} =
  case extractPlayer name _gamePlayers of
    Nothing -> g
    Just (p, ps) -> 
      g & gamePlayers .~ ps                     -- remove the player
        & gameFreeColors %~ (_playerCol p :)    -- free the player color

loadBoard :: MisoString -> Game -> Maybe Game
loadBoard str g =
  case parseBoard str of
    Nothing -> Nothing
    Just b -> Just $ g & gameBoard ?~ b
                       & gameRemMeetings .~ nbMeetings


playMove :: Int -> Move -> Game -> Maybe Game
playMove _ _ g = Just g

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

-- TODO test
extractPlayer :: MisoString -> [Player] -> Maybe (Player, [Player])
extractPlayer name = go []
  where
    go _ [] = Nothing
    go acc (p:ps) =
      if _playerName p == name
        then Just (p, acc++ps)
        else go (acc++[p]) ps

