{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Protocol where

import Data.ByteString.Lazy
import Data.Text.Encoding as T
import Data.Aeson as Aeson
import GHC.Generics
import Miso

import Game.Common

import Language.Javascript.JSaddle    -- TODO

data ServerMsg
  = Rejected MisoString
  | UpdateBoard MisoString
  | UpdatePlayers [Player]
  deriving (Eq, Generic, Show)

instance ToJSON ServerMsg where
    toEncoding = genericToEncoding Aeson.defaultOptions

instance FromJSON ServerMsg

data ClientMsg
  = Join MisoString
  | Play Move
  deriving (Eq, Generic, Show)

instance ToJSON ClientMsg where
    toEncoding = genericToEncoding Aeson.defaultOptions

instance FromJSON ClientMsg

instance ToJSVal ClientMsg where
  toJSVal = valMakeJSON . toJSON

{-
#ifdef WASM
#else
instance ToJSVal ClientMsg where
  toJSVal = valMakeText . decodeUtf8 . toStrict . encode 
#endif
-}

