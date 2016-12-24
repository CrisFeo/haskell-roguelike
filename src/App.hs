{-# LANGUAGE OverloadedStrings #-}
-- TODO Document
module App
  ( run
  ) where

import Brick (App (..), on, fg, withAttr, str)
import Brick.AttrMap (attrMap, AttrMap, AttrName)
import Brick.Main (continue, halt, neverShowCursor, customMain)
import Brick.Types (BrickEvent (VtyEvent), EventM (EventM), Next, Widget)
import Brick.Widgets.Core (hBox, vBox)
import Brick.Widgets.Center (center)
import Control.Concurrent (Chan, newChan, writeChan)
import qualified Graphics.Vty as V
import Lens.Micro.Platform

import Dungeon (dungeonMap, renderDungeon)
import State (St (St), enemyPos, playerPos)
import Enemy (handleEnemyEvents, placeEnemy)
import Events (BrickGameEvent, GameEvent, GameEventHandler, HandlerResult, runHandlers)
import Player (handlePlayerEvents, placePlayer)

black :: V.Color
black = V.rgbColor 0 0 0

gray :: V.Color
gray = V.rgbColor 135 135 135

green :: V.Color
green = V.rgbColor 114 193 176

red :: V.Color
red = V.rgbColor 254 67 101

white :: V.Color
white = V.rgbColor 255 255 255

appAttrs :: AttrMap
appAttrs = attrMap (white `on` black)
                   [ ("Wall", black `on` gray )
                   , ("Floor", gray `on` black)
                   , ("Player", green `on` black)
                   , ("Enemy", red `on` black) ]

drawUI :: St -> [Widget ()]
drawUI st =  [ center . renderDungeon . placeObjects $ dungeonMap ]
  where placeObjects = placeEnemy (st ^. enemyPos) . placePlayer (st ^. playerPos)

handleAppEvents :: GameEventHandler
handleAppEvents ch st ev =
  case ev of
       VtyEvent (V.EvKey V.KEsc []) -> Right . halt $ st
       _                            -> Left . return $ st

handleEvent :: Chan GameEvent -> St -> BrickGameEvent -> HandlerResult
handleEvent ch = runHandlers ch [ handleAppEvents
                                , handlePlayerEvents
                                , handleEnemyEvents ]

app :: Chan GameEvent -> App St GameEvent ()
app ch = App { appDraw = drawUI
             , appHandleEvent = handleEvent ch
             , appAttrMap = const appAttrs
             , appStartEvent = return
             , appChooseCursor = neverShowCursor }

initialState :: St
initialState = St (5, 5) (14, 12)

run :: IO St
run = do
  eventChan <- newChan
  customMain (V.mkVty $ mempty V.Config) (Just eventChan) (app eventChan) initialState
