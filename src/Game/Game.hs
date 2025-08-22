{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Game where

import Data.Map qualified as M
import Data.Vector qualified as V
import Miso hiding (go)
import Miso.Lens
import Miso.Lens.TH
import System.Random

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
  , _gameGen :: StdGen
  } deriving (Eq)

makeLenses ''Game

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

mkGame :: StdGen -> Game
mkGame gen = Game emptyBoard M.empty playerColors nbMeetings gen

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
      g & gameInfoPlayer %~ M.delete name       -- remove the player
        & gameFreeColors %~ (_infoCol info :)   -- free the player color

loadBoard :: MisoString -> Game -> Maybe Game
loadBoard str g =
  case parseBoard str of
    Nothing -> Nothing
    Just b -> Just $ g & gameBoard .~ b
                       & gameRemMeetings .~ nbMeetings
  -- TODO reset player positions randomly


playMove :: MisoString -> Move -> Game -> Maybe Game
playMove name move g =
  case (M.lookup name (_gameInfoPlayer g), isRunning g) of
    (Just info, True) -> 
      case move of
        MoveLeft  -> go info $ Position 0 (-1)
        MoveRight -> go info $ Position 0 1
        MoveUp    -> go info $ Position (-1) 0
        MoveDown  -> go info $ Position 1 0
    _ -> Nothing

  where
    go :: InfoPlayer -> Position -> Maybe Game
    go info dij = 
      case tryMove (_gameBoard g) (_infoPos info) dij of
        Nothing -> Nothing
        Just ij1 ->
          if isMeeting name ij1 g
            then Just $ g & gameRemMeetings %~ subtract 1
              -- TODO reset player positions
            else Just $ g & gameInfoPlayer %~ M.adjust (\i -> i & infoPos .~ ij1) name

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

tryMove :: Board -> Position -> Position -> Maybe Position
tryMove Board{..} ij0 dij =
  if i1 >= 0 && i1 < ni && j1 >= 0 && j1 < nj && c == CellEmpty
    then Just ij1
    else Nothing
  where
    (ni, nj) = _boardNiNj
    ij1@(Position i1 j1) = ij0 + dij
    c = _boardData V.! ij2k nj (i1, j1)

isMeeting :: MisoString -> Position -> Game -> Bool
isMeeting name ij1 Game{..} = M.foldrWithKey' f False _gameInfoPlayer
  where
    f n (InfoPlayer ij _) acc = acc || n/=name && ij==ij1

