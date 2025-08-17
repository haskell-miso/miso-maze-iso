{-# LANGUAGE RecordWildCards #-}

module Domain.Game where

import Data.Map as M
import Miso
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Lens
import Miso.Lens.TH
import Data.Vector qualified as V

data Position = Position
  { _posI :: Int
  , _posJ :: Int
  } deriving (Eq)

makeLenses ''Position

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  deriving (Eq)

data Cell
  = CellEmpty
  | CellWall
  deriving (Eq)

data Board = Board
  { _boardNi :: Int
  , _boardNj :: Int
  , _boardData :: V.Vector Cell
  } deriving (Eq)

makeLenses ''Board

data Infos = Infos
  { _infosOthers :: [Position]
  , _infosCurrent :: Position
  , _infosScore :: Int
  } deriving (Eq)

makeLenses ''Infos

data Game = Game
  { _gameBoard :: Board
  , _gamePlayers :: Map MisoString Position
  , _gameScore :: Int
  } deriving (Eq)

makeLenses ''Game

mkInfos :: Infos
mkInfos = Infos [] (Position 1 1) 0     -- TODO

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

forBoard :: (Monad m) => Board -> (Int -> Int -> Cell -> m ()) -> m ()
forBoard Board{..} f = 
  V.iforM_ _boardData $ \k c -> 
    let (i, j) = k2ij _boardNj k
    in f i j c

ij2k :: Int -> (Int, Int) -> Int
ij2k nj (i, j) = i*nj + j

k2ij :: Int -> Int -> (Int, Int)
k2ij nj k = (k `div` nj, k`rem` nj)
  
