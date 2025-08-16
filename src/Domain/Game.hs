{-# LANGUAGE RecordWildCards #-}

module Domain.Game where

import Data.Map as M
import Miso
-- import Miso.Lens
-- import Miso.Lens.TH
import Data.Vector qualified as V

data Position = Position
  { _posI :: Int
  , _posJ :: Int
  } deriving (Eq)

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

-- makeLenses ''Position

-- playerI :: Lens Player Int
-- playerI = lens _playerI (\ record field -> record {_playerI = field})

data Cell
  = CellEmpty
  | CellWall
  deriving (Eq)

data Board = Board
  { _boardNi :: Int
  , _boardNj :: Int
  , _boardData :: V.Vector Cell
  } deriving (Eq)

data Game = Game
  { _gameBoard :: Board
  , _gamePlayers :: Map MisoString Position
  , _gameScore :: Int
  } deriving (Eq)

data Infos = Infos
  { _infosOthers :: [Position]
  , _infosCurrent :: Position
  , _infosScore :: Int
  }

playMove :: MisoString -> Move -> Game -> Maybe Game
playMove _ _ g = Just g

getInfos :: MisoString -> Game -> Maybe Infos
getInfos name Game{..} =
  case name `M.lookup` _gamePlayers of
    Nothing -> Nothing
    Just p -> Just $ Infos (elems _gamePlayers) p _gameScore

mkBoard :: Board    -- TODO
mkBoard = Board 8 10 $ V.fromList
  [ CellWall, CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellWall,  CellWall,  CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellWall,  CellWall,  CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall
  ]
  
