{-# LANGUAGE RecordWildCards #-}

module Game.Board where

import Data.Vector qualified as V
import Miso
import Miso.Lens
import Miso.Lens.TH

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

forBoard :: (Monad m) => Board -> (Int -> Int -> Cell -> m ()) -> m ()
forBoard Board{..} f = 
  V.iforM_ _boardData $ \k c -> 
    let (i, j) = k2ij _boardNj k
    in f i j c

ij2k :: Int -> (Int, Int) -> Int
ij2k nj (i, j) = i*nj + j

k2ij :: Int -> Int -> (Int, Int)
k2ij nj k = (k `div` nj, k`rem` nj)
  

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

