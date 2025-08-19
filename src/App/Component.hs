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
import Miso.WebSocket

import App.Model 
import Game.Common
import Network.Api

import Language.Javascript.JSaddle

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
    | ActionWsOpen MisoString WebSocket
    | ActionWsClosed Closed
    | ActionWsMessage JSVal
    | ActionWsError JSVal
    | ActionSend WebSocket JSVal

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
      wsUrl <- use modelWsUrl
      io_ (consoleLog wsUrl)  -- TODO
      connect wsUrl (ActionWsOpen inputName) ActionWsClosed ActionWsMessage ActionWsError
      -- TODO
      fetch (mkStaticUri "maze1.json") "GET" Nothing [] ActionSetBoard ActionError
      fetch (mkStaticUri "infos1.json") "GET" Nothing [] ActionSetPlayers ActionError

updateModel (ActionSetBoard str) =
  case parseBoard str of
    Nothing -> modelError .= "error: failed to parse board"
    Just board -> modelBoard .= Just board

updateModel (ActionSetPlayers players) =
  modelPlayers .= players

updateModel (ActionWsOpen name socket) =
  io (ActionSend socket <$> toJSVal name)

updateModel (ActionSend socket val) =
  send socket val

updateModel (ActionWsClosed _) =
  pure ()

updateModel (ActionWsMessage _) =
  pure ()

updateModel (ActionWsError _) =
  pure ()

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
              , CSS.style_  [CSS.border "1px solid black"]
              ]
            initCanvas
            (drawCanvas w h m)
          , div_ [] [ "Board name: " ]    -- TODO
          , div_ [] [ "Remaining meetings: " ]  -- TODO
          , div_ [] [ "Players:" ]
          , div_ [ CSS.style_ [ CSS.marginLeft "10px" ] ] (map fmtPlayer' players)
          ]

    fmtPlayer' Player{..} =
        div_
            [ CSS.style_ 
                [ CSS.display "flex"
                , CSS.flexWrap "wrap"
                , CSS.verticalAlign "middle"
                , CSS.alignItems "center"
                ]
            ]
            [ div_
                [ CSS.style_ 
                    [ CSS.backgroundColor (CSS.hex _playerCol)
                    , CSS.borderColor backgroundColor
                    , CSS.marginRight "5px" 
                    , CSS.border "5px solid" 
                    , CSS.padding "10px" 
                    ]
                 ]
                [ " " ]
            , text _playerName
            ]

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Double -> Double -> Model -> () -> Canvas ()
drawCanvas w h Model{..} () = do
  Canvas.clearRect (0, 0, w, h)

  Canvas.fillStyle (Canvas.color backgroundColor)
  Canvas.fillRect (0, 0, w, h)

  Canvas.fillStyle (Canvas.color wallColor)
  forM_ _modelBoard (`forBoard` drawBoardCell)

  forM_ _modelPlayers $ \Player{..} -> do
    Canvas.fillStyle (Canvas.color (CSS.hex _playerCol))
    drawCell _playerPos


drawBoardCell :: Int -> Int -> Cell -> Canvas ()
drawBoardCell i j = \case
  CellEmpty -> pure ()
  CellWall  -> drawCell (Position i j)

drawCell :: Position -> Canvas ()
drawCell (Position i j) =
  Canvas.fillRect (cellSizeD * fromIntegral j, cellSizeD * fromIntegral i, cellSizeD, cellSizeD)

cellSizeD :: Double
cellSizeD = fromIntegral cellSize

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------

type AppComponent = App Model Action

appComponent :: MisoString -> AppComponent
appComponent wsUrl = do
  let initialModel = mkModel wsUrl
  (component initialModel updateModel viewModel)
    { logLevel = DebugAll
    }

