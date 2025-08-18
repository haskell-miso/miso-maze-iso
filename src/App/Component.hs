{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Control.Monad
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
    | ActionBoard MisoString
    deriving (Eq)

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel (ActionError str) =
  modelError .= str

updateModel (ActionBoard filename) =
  -- fetch (mkStaticUri filename)
  pure () -- TODO

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
    , viewBoard _modelBoard
    ]

  where
    viewBoard = \case
      Nothing -> span_ [] []
      Just Board{..} ->
        let 
          w = fromIntegral $ cellSize * _boardNj
          h = fromIntegral $ cellSize * _boardNi
        in canvas 
              [ width_ (ms $ show w)
              , height_ (ms $ show h)
              , Style.style_  [Style.border "2px solid black"]
              ]
            initCanvas
            (drawCanvas w h m)

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
    { logLevel = DebugAll
    , initialAction = Just (ActionBoard "maze1.txt")
    }

