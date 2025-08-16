{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Miso
import Miso.Lens

import App.Model 
import Domain.Hero

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action
    = ActionPopHeroes
    | ActionError MisoString
    | ActionFetchFail
    | ActionFetchHeroes
    | ActionSetHeroes [Hero]
    deriving (Eq)

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel ActionPopHeroes = do
  modelHeroes %= \case
    [] -> []
    (_:xs) -> xs
  modelError .= ""

updateModel (ActionError str) =
  modelError .= str

updateModel ActionFetchFail =
  fetch "fail" "GET" Nothing [] ActionSetHeroes ActionError

updateModel ActionFetchHeroes =
  fetch "./heroes" "GET" Nothing [] ActionSetHeroes ActionError

updateModel (ActionSetHeroes heroes)  = do
  modelHeroes .= heroes
  modelError .= ""

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel Model{..} =
  div_ 
    []
    [ h1_ [] [ "miso-maze" ]
    , p_ []
        [ button_ [ onClick ActionFetchFail ] [ "fetch fail" ]
        , button_ [ onClick ActionFetchHeroes ] [ "fetch heroes" ]
        , button_ [ onClick ActionPopHeroes ] [ "pop heroes" ]
        ]
    , ul_ [] (map fmtHero _modelHeroes)
    , p_ [] [ text _modelError ]
    ]

  where
    fmtHero Hero{..} =
      li_ []
        [ text _heroName
        ]

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------

type AppComponent = App Model Action

appComponent :: AppComponent
appComponent = do
  let initialModel = mkModel
  (component initialModel updateModel viewModel)
    { logLevel = DebugAll }

