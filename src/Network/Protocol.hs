
module Network.Protocol where

data ServerMsg
  = UpdateBoard
  | UpdateInfos
  deriving (Eq)

data ClientMsg
  = Join
  | Play
  | Quit
  deriving (Eq)

