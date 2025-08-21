{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Control.Monad (forM_)
import Data.IntSet qualified as IS
import Miso
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Lens
import Miso.CSS qualified as CSS
import Miso.String (strip)
import Miso.WebSocket

import App.Model 
import Game.Common
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
-- Action
-------------------------------------------------------------------------------

data Action
  = ActionNone
  | ActionDebug MisoString
  | ActionKey IS.IntSet
  | ActionName MisoString
  | ActionConnect
  | ActionSetBoard MisoString
  | ActionWsOpen WebSocket
  | ActionWsClosed Closed
  | ActionWsError MisoString
  | ActionWsRecv ServerMsg
  | ActionWsSend ClientMsg

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

debug :: MisoString -> Transition Model Action
debug str = io_ (consoleLog str)

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel ActionNone =
  pure ()

updateModel (ActionDebug str) =
  debug str

updateModel (ActionKey keys)
  | IS.member 37 keys = issue $ ActionWsSend (Play MoveLeft)
  | IS.member 38 keys = issue $ ActionWsSend (Play MoveUp)
  | IS.member 39 keys = issue $ ActionWsSend (Play MoveRight)
  | IS.member 40 keys = issue $ ActionWsSend (Play MoveDown)
  | otherwise = pure ()

updateModel (ActionName str) =
  modelName .= str

updateModel ActionConnect = do
  name <- strip <$> use modelName
  if name == ""
    then modelError .= "Please, enter a valid name."
    else do
      modelError .= ""
      wsUrl <- use modelWsUrl
      connectJSON wsUrl ActionWsOpen ActionWsClosed ActionWsRecv ActionWsError

updateModel (ActionSetBoard str) =
  case parseBoard str of
    Nothing -> modelError .= "Error: failed to parse board."
    Just board -> modelBoard .= Just board

updateModel (ActionWsOpen socket) = do
  modelWs .= socket
  name <- strip <$> use modelName
  issue $ ActionWsSend (Join name)

updateModel (ActionWsClosed c) = do
  debug (ms $ show c)

updateModel (ActionWsError err) = do
  modelError .= "WebSocket error"
  debug err

updateModel (ActionWsRecv json) =
  case json of
    Rejected err -> modelError .= "Error: " <> err
    UpdatePlayers ps -> modelPlayers .= ps
    UpdateBoard url -> do
      debug url 
      getText url [] ActionSetBoard ActionDebug
      -- fetch url "GET" Nothing [] ActionSetBoard ActionDebug

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
    , maybe viewConnect viewRun _modelBoard
    , p_ [] [ text _modelError ]
    ]

  where
    viewConnect =
      div_ []
        [ "Name: "
        , input_ [ type_ "text", onChange ActionName ]
        , button_ [ onClick ActionConnect ] [ "connect" ]
        ]

    viewRun Board{..} =
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
          , div_ [] [ "Player name: ", text _modelName ]
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
  Canvas.fillRect 
    ( cellSizeD * fromIntegral j
    , cellSizeD * fromIntegral i
    , cellSizeD
    , cellSizeD
    )

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
    { subs = [ keyboardSub ActionKey ]
    , logLevel = DebugAll
    }

