{-# LANGUAGE OverloadedStrings #-}

module Network.Api where

import Data.Proxy
import Miso.String
import Servant.API
import Servant.Links

type StaticApi = "pub" :> Raw

type PublicApi
  =    StaticApi 

uriStatic :: URI
uriStatic  = allLinks' linkURI (Proxy @PublicApi)

mkStaticUri :: MisoString -> MisoString
mkStaticUri filename = ms (show uriStatic) <> "/" <> filename

