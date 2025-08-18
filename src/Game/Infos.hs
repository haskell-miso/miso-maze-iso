
module Game.Infos where

import Miso.Lens
import Miso.Lens.TH

import Game.Position

data Infos = Infos
  { _infosOthers :: [Position]
  , _infosCurrent :: Position
  , _infosScore :: Int
  } deriving (Eq)

makeLenses ''Infos

mkInfos :: [Position] -> Position -> Int -> Infos
mkInfos = Infos

