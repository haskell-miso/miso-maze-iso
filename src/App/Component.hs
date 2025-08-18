{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Control.Monad
import Miso
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Lens
import Miso.CSS qualified as CSS
import Miso.String (strip)

import App.Model 
import Game.Common
import Network.Api

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

cellSize :: Int
cellSize = 20

backgroundColor, wallColor :: CSS.Color
backgroundColor   = CSS.hex 0x88DD88
wallColor         = CSS.hex 0x2277DD

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action
    = ActionError MisoString
    | ActionInputName MisoString
    | ActionConnect
    | ActionSetBoard MisoString
    | ActionSetPlayers [Player]
    deriving (Eq)

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel (ActionError str) =
  modelError .= str

updateModel (ActionInputName str) =
  modelInputName .= str

updateModel ActionConnect = do
  inputName <- strip <$> use modelInputName
  if inputName == ""
    then modelError .= "Please, enter a valid name."
    else do
      modelError .= ""
      -- TODO
      fetch (mkStaticUri "maze1.json") "GET" Nothing [] ActionSetBoard ActionError
      fetch (mkStaticUri "infos1.json") "GET" Nothing [] ActionSetPlayers ActionError

updateModel (ActionSetBoard str) =
  case parseBoard str of
    Nothing -> modelError .= "error: failed to parse board"
    Just board -> modelBoard .= Just board

updateModel (ActionSetPlayers players) =
  modelPlayers .= players

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel m@Model{..} =
  div_ 
    []
    [ h1_ [] [ "miso-maze" ]
    , viewMain
    , p_ [] [ text _modelError ]
    ]

  where
    viewMain =
      case (_modelPlayers, _modelBoard) of
        (players@(_:_), Just board) -> viewRunning players board
        _ -> viewStarting

    viewStarting =
      div_ []
        [ "Name: "
        , input_ [ type_ "text", onChange ActionInputName ]
        , button_ [ onClick ActionConnect ] [ "connect" ]
        ]

    viewRunning players Board{..} =
      let 
        w = fromIntegral $ cellSize * _boardNj
        h = fromIntegral $ cellSize * _boardNi
      in div_ []
          [ p_ [] [ "Meet other players, using keyboard arrows." ]
          , canvas 
              [ width_ (ms $ show w)
              , height_ (ms $ show h)
              , CSS.style_  [CSS.border "2px solid black"]
              ]
            initCanvas
            (drawCanvas w h m)
          , p_ [] 
              [ "Players:"
              , div_
                  [ CSS.style_ 
                      [ CSS.display "flex"
                      , CSS.flexWrap "wrap"
                      ]
                  ]
                  (map fmtPlayer players)
              ]
          ]

    fmtPlayer Player{..} =
      div_
        [ CSS.style_ 
            [ CSS.backgroundColor backgroundColor
            , CSS.fontWeight "bold" 
            , CSS.margin "10px" 
            , CSS.padding "10px" 
            , CSS.color (CSS.hex _playerCol)
            ]
        ]
        [ text _playerName ]

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Double -> Double -> Model -> () -> Canvas ()
drawCanvas w h m () = do
  Canvas.clearRect (0, 0, w, h)

  Canvas.fillStyle (Canvas.color backgroundColor)
  Canvas.fillRect (0, 0, w, h)

  forM_ (m^.modelBoard) $ \b -> do
    Canvas.fillStyle (Canvas.color wallColor)
    forBoard b drawBoardCell 

{-
  Canvas.fillStyle (Canvas.color currentColor)
  drawCell 1 1

  Canvas.fillStyle (Canvas.color otherColor)
  drawCell 1 3
  -- TODO current, other
-}


drawBoardCell :: Int -> Int -> Cell -> Canvas ()
drawBoardCell i j = \case
  CellEmpty -> pure ()
  CellWall  -> drawCell i j

drawCell :: Int -> Int -> Canvas ()
drawCell i j =
  Canvas.fillRect (cellSizeD * fromIntegral j, cellSizeD * fromIntegral i, cellSizeD, cellSizeD)

cellSizeD :: Double
cellSizeD = fromIntegral cellSize



-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------

type AppComponent = App Model Action

appComponent :: AppComponent
appComponent = do
  let initialModel = mkModel
  (component initialModel updateModel viewModel)
    { logLevel = DebugAll
    }

