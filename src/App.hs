{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- TODO Document
module App
  ( run
  ) where

import Brick (App (..), on)
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Main (halt, neverShowCursor, customMain)
import Brick.Types (BrickEvent (VtyEvent), Widget)
import Brick.Widgets.Center (center)
import Control.Concurrent (Chan, newChan)
import Graphics.Vty (Color, Config (Config), Event (EvKey), Key (KEsc), mkVty, rgbColor)
import Lens.Micro.Platform

import Dungeon (dungeonMap, renderDungeon)
import State (St (St), enemyPos, playerPos)
import Enemy (enemyTile, handleEnemyEvents)
import Events (BrickGameEvent, GameEvent, GameEventHandler, HandlerResult, runHandlers)
import Player (playerTile, handlePlayerEvents)

black :: Color
black = rgbColor (0::Int) (0::Int) (0::Int)

gray :: Color
gray = rgbColor (135::Int) (135::Int) (135::Int)

green :: Color
green = rgbColor (114::Int) (193::Int) (176::Int)

red :: Color
red = rgbColor (254::Int) (67::Int) (101::Int)

white :: Color
white = rgbColor (255::Int) (255::Int) (255::Int)

appAttrs :: AttrMap
appAttrs = attrMap (white `on` black)
                   [ ("Wall", black `on` gray )
                   , ("Floor", gray `on` black)
                   , ("Player", green `on` black)
                   , ("Enemy", red `on` black) ]

drawUI :: St -> [Widget ()]
drawUI st =  [ center . renderDungeon . placePlayer . placeEnemy $ dungeonMap ]
  where placeEnemy g = g & ix (st ^. enemyPos) .~ enemyTile
        placePlayer g = g & ix (st ^. playerPos) .~ playerTile

handleAppEvents :: GameEventHandler
handleAppEvents _ st ev =
  case ev of
       VtyEvent (EvKey KEsc []) -> Right . halt $ st
       _                        -> Left . return $ st

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
  customMain (mkVty $ mempty Config) (Just eventChan) (app eventChan) initialState
