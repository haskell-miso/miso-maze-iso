{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Miso
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Lens
import Miso.Style qualified as Style

import App.Model 
import Game.Board
import Game.Game

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

cellSize :: Int
cellSize = 20

backgroundColor, wallColor, currentColor, otherColor :: Style.Color
backgroundColor   = Style.Hex "88DD88"
wallColor         = Style.Hex "2277DD"
currentColor      = Style.Hex "FF0000"
otherColor        = Style.Hex "666666"

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action
    = ActionError MisoString
    deriving (Eq)

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel (ActionError str) =
  modelError .= str

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel m@Model{..} =
  div_ 
    []
    [ h1_ [] [ "miso-maze" ]
    , p_ []
        [ button_ [ onClick (ActionError "error")  ] [ "launch error" ]
        ]
    , p_ [] [ text _modelError ]
    , canvas 
        [ width_ (ms $ show w)
        , height_ (ms $ show h)
        , Style.style_  [Style.border "2px solid black"]
        ]
      initCanvas
      (drawCanvas w h m)
    ]

  where
    w = fromIntegral $ cellSize * m^.modelBoard^.boardNj
    h = fromIntegral $ cellSize * m^.modelBoard^.boardNi

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Double -> Double -> Model -> () -> Canvas ()
drawCanvas w h m () = do
  Canvas.clearRect (0, 0, w, h)

  Canvas.fillStyle (Canvas.color backgroundColor)
  Canvas.fillRect (0, 0, w, h)

  Canvas.fillStyle (Canvas.color wallColor)
  forBoard (m^.modelBoard) drawBoardCell 

  Canvas.fillStyle (Canvas.color currentColor)
  drawCell 1 1

  Canvas.fillStyle (Canvas.color otherColor)
  drawCell 1 3
  -- TODO current, other


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
    { logLevel = DebugAll }

