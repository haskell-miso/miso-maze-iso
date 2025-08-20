{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Control.Monad
import Data.Bool
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
import Network.Protocol

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

cellSize :: Int
cellSize = 20

backgroundColor, wallColor :: CSS.Color
backgroundColor   = CSS.hex 0x88DD88
wallColor         = CSS.hex 0x2277DD

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

debug :: MisoString -> Transition Model Action
debug str = io_ (consoleLog str)

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool ActionNone action . (== KeyCode 13)
-- TODO fix keydown events
-- App -> Component?
-- miso -> startApp

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action
    = ActionNone
    | ActionDebug MisoString
    | ActionInputName MisoString
    | ActionConnect
    | ActionSetBoard MisoString
    | ActionWsOpen MisoString WebSocket   -- client name
    | ActionWsClosed Closed
    | ActionWsError MisoString
    | ActionWsRecv (Payload ServerMsg)
    | ActionWsSend ClientMsg    -- TODO put socket into the model

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel ActionNone =
  pure ()

updateModel (ActionDebug str) =
  debug str

updateModel (ActionInputName str) =
  modelInputName .= str

updateModel ActionConnect = do
  inputName <- strip <$> use modelInputName
  if inputName == ""
    then modelError .= "Please, enter a valid name."
    else do
      modelError .= ""
      wsUrl <- use modelWsUrl
      connect wsUrl (ActionWsOpen inputName) ActionWsClosed ActionWsRecv ActionWsError

updateModel (ActionSetBoard str) =
  case parseBoard str of
    Nothing -> modelError .= "Error: failed to parse board."
    Just board -> modelBoard .= Just board

updateModel (ActionWsOpen name socket) = do
  modelWs .= socket
  issue $ ActionWsSend (Join name)

updateModel (ActionWsClosed c) = do
  modelError .= ms (show c)
  debug (ms $ show c)

updateModel (ActionWsError err) = do
  modelError .= "Error: failed to connect websocket."
  debug err

  -- TODO
updateModel (ActionWsRecv payload) =
  case payload of
    JSON json -> case json of
      UpdatePlayers ps -> 
        modelPlayers .= ps
      UpdateBoard filename -> 
        fetch (mkStaticUri filename) "GET" Nothing [] ActionSetBoard ActionDebug
      Rejected err -> modelError .= "Error: " <> err
    _ -> debug "ActionWsRecv: unexpected data type"

updateModel (ActionWsSend msg) = do
  socket <- use modelWs
  sendJSON socket msg

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel m@Model{..} =
  div_ 
    []
    [ h1_ [] [ "miso-maze" ]
    , maybe viewStarting viewRunning _modelBoard
    , p_ [] [ text _modelError ]
    ]

  where
    viewStarting =
      div_ []
        [ "Name: "
        , input_ [ type_ "text", onChange ActionInputName ]
        , button_ [ onClick ActionConnect, onEnter ActionConnect ] [ "connect" ]
        ]

    viewRunning Board{..} =
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
          , div_ [ CSS.style_ [ CSS.marginLeft "10px" ] ] (map fmtPlayer' _modelPlayers)
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
    { events = defaultEvents <> keyboardEvents
    , logLevel = DebugAll
    }

