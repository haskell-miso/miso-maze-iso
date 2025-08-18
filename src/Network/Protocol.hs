{-# LANGUAGE DeriveGeneric #-}

module Network.Protocol where

import Data.Aeson as Aeson
import GHC.Generics
import Miso

data ServerMsg
  = Rejected MisoString
  | UpdateBoard
  | UpdateInfos
  deriving (Eq, Generic)

instance ToJSON ServerMsg where
    toEncoding = genericToEncoding Aeson.defaultOptions

instance FromJSON ServerMsg

data ClientMsg
  = Join
  | Play
  | Quit
  deriving (Eq, Generic)

instance ToJSON ClientMsg where
    toEncoding = genericToEncoding Aeson.defaultOptions

instance FromJSON ClientMsg

