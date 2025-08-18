
module Game.Position where

import Miso.Lens
import Miso.Lens.TH

data Position = Position
  { _posI :: Int
  , _posJ :: Int
  } deriving (Eq)

makeLenses ''Position


