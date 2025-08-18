{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Control.Monad
import Miso
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Lens
import Miso.Style qualified as Style
import Miso.String (strip)

import App.Model 
import Game.Common
import Server.Api

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
    | ActionInputName MisoString
    | ActionConnect
    | ActionParseBoard MisoString
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
      modelName .= Just inputName
      fetch (mkStaticUri "maze1.json") "GET" Nothing [] ActionParseBoard ActionError

updateModel (ActionParseBoard str) =
  case parseBoard str of
    Nothing -> modelError .= "error: failed to parse board"
    Just b -> modelBoard .= Just b

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
      case (_modelName, _modelBoard) of
        (Just name, Just board) -> viewBoard name board
        (Just name, Nothing) -> viewName name
        _ -> viewStart

    viewBoard name Board{..} =
      let 
        w = fromIntegral $ cellSize * _boardNj
        h = fromIntegral $ cellSize * _boardNi
      in div_ []
          [ p_ [] 
              [ text ("You are: " <> name) 
              , br_ []
              , text ("Score: " <> ms (show $ m^.modelInfos^.infosScore))
              , br_ []
              , "Use keyboard arrows and meet other players."
              ]
          , canvas 
              [ width_ (ms $ show w)
              , height_ (ms $ show h)
              , Style.style_  [Style.border "2px solid black"]
              ]
            initCanvas
            (drawCanvas w h m)
          ]

    viewName name =
      div_ []
        [ p_ [] [ text ("You are: " <> name) ]
        , p_ [] [ "Waiting server..." ]
        ]

    viewStart =
      div_ []
        [ "Name: "
        , input_ [ type_ "text", onChange ActionInputName ]
        , button_ [ onClick ActionConnect ] [ "connect" ]
        ]

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
    }

