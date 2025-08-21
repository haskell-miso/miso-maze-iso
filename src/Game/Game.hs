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
  { _gameBoard :: Board
  , _gamePlayers :: [Player]    -- TODO M.Map MisoString (Position, Int)
  , _gameFreeColors :: [Int]
  , _gameRemMeetings :: Int
  } deriving (Eq)

makeLenses ''Game

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

mkGame :: Game
mkGame = Game emptyBoard [] playerColors nbMeetings

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
    Just b -> Just $ g & gameBoard .~ b
                       & gameRemMeetings .~ nbMeetings
  -- TODO reset player positions


playMove :: MisoString -> Move -> Game -> Maybe Game
playMove name move g =
  case (filter ((==name) . _playerName) (g^.gamePlayers), isRunning g) of
    ([player], True) -> 
      case move of
        MoveLeft  -> go player ( 0, -1)
        MoveRight -> go player ( 0,  1)
        MoveUp    -> go player (-1,  0)
        MoveDown  -> go player ( 1,  0)
    _ -> Nothing

  where
    go :: Player -> (Int, Int) -> Maybe Game
    go p (di, dj) = 
      let ni = g ^. gameBoard ^. boardNi
          nj = g ^. gameBoard ^. boardNj
          (Position i0 j0) = p ^. playerPos
          i1 = i0 + di
          j1 = j0 + dj
          upPos pp = if pp^.playerName == name then pp&playerPos .~ Position i1 j1 else pp
      in if i1 >= 0 && i1 < ni && j1 >= 0 && j1 < nj
        then Just $ g & gamePlayers %~ map upPos
        else Nothing

  -- TODO

isRunning :: Game -> Bool
isRunning Game{..} = _gameRemMeetings > 0

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

