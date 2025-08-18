
module Network.Protocol where

import Data.Aeson
import Miso

data ServerMsg
  = Rejected MisoString
  | UpdateBoard
  | UpdateInfos
  deriving (Eq)

data ClientMsg
  = Join
  | Play
  | Quit
  deriving (Eq)

