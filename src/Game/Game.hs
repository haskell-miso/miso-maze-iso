{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Game where

import Data.Map qualified as M
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

data InfoPlayer = InfoPlayer
  { _infoPos :: Position
  , _infoCol :: Int
  } deriving (Eq)

makeLenses ''InfoPlayer

data Game = Game
  { _gameBoard :: Board
  , _gameInfoPlayer :: M.Map MisoString InfoPlayer
  , _gameFreeColors :: [Int]
  , _gameRemMeetings :: Int
  } deriving (Eq)

makeLenses ''Game

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

mkGame :: Game
mkGame = Game emptyBoard M.empty playerColors nbMeetings

tryAddPlayer :: MisoString -> Game -> Either MisoString Game
tryAddPlayer name g@Game{..} =
  case _gameFreeColors of
    [] -> Left "the maze is full"
    (c:cs) -> 
      if M.member name _gameInfoPlayer
        then Left $ name <> " is already in the maze"
        else Right $ g & gameFreeColors .~ cs
                       & gameInfoPlayer %~ M.insert name (InfoPlayer pos c)
  where
    pos = Position 1 1  -- TODO random

deletePlayer :: MisoString -> Game -> Game
deletePlayer name g@Game{..} =
  case M.lookup name _gameInfoPlayer of
    Nothing -> g
    Just info -> 
      g & gameInfoPlayer %~ M.delete name          -- remove the player
        & gameFreeColors %~ (_infoCol info :)   -- free the player color

loadBoard :: MisoString -> Game -> Maybe Game
loadBoard str g =
  case parseBoard str of
    Nothing -> Nothing
    Just b -> Just $ g & gameBoard .~ b
                       & gameRemMeetings .~ nbMeetings
  -- TODO reset player positions


playMove :: MisoString -> Move -> Game -> Maybe Game
playMove name move g =
  case (M.lookup name (_gameInfoPlayer g), isRunning g) of
    (Just info, True) -> 
      case move of
        MoveLeft  -> go info ( 0, -1)
        MoveRight -> go info ( 0,  1)
        MoveUp    -> go info (-1,  0)
        MoveDown  -> go info ( 1,  0)
    _ -> Nothing

  where
    go :: InfoPlayer -> (Int, Int) -> Maybe Game
    go info (di, dj) = 
      let ni = g ^. gameBoard ^. boardNi
          nj = g ^. gameBoard ^. boardNj
          (Position i0 j0) = _infoPos info
          i1 = i0 + di
          j1 = j0 + dj
      in if i1 >= 0 && i1 < ni && j1 >= 0 && j1 < nj
        then Just $ g & gameInfoPlayer %~ M.adjust (\i -> i & infoPos .~ Position i1 j1) name
        else Nothing

  -- TODO

isRunning :: Game -> Bool
isRunning Game{..} = _gameRemMeetings > 0

getPlayers :: Game -> [Player]
getPlayers Game{..} = M.foldrWithKey' f [] _gameInfoPlayer
  where
    f name (InfoPlayer pos col) acc = Player name col pos : acc

getPlayerNames :: Game -> [MisoString]
getPlayerNames = M.keys . _gameInfoPlayer

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

