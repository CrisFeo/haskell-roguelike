{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- TODO Document
module App
  ( run
  ) where

import           Brick                (App (..), on)
import           Brick.AttrMap        (AttrMap, attrMap)
import           Brick.Main           (customMain, halt, neverShowCursor)
import           Brick.Types          (BrickEvent (VtyEvent), Padding (..),
                                       Widget)
import           Brick.Widgets.Border (borderWithLabel)
import           Brick.Widgets.Center (center, hCenter)
import           Brick.Widgets.Core   (hBox, hLimit, padLeft, str, vBox,
                                       withAttr, withBorderStyle)
import           Control.Concurrent   (Chan, newChan)
import           Graphics.Vty         (Color, Config (Config), Event (EvKey),
                                       Key (KEsc), mkVty, rgbColor)
import           Lens.Micro.Platform

import           Dungeon              (Tile, dungeonMap, renderDungeon)
import           Enemy                (enemyTile, handleEnemyEvents)
import           Events               (BrickGameEvent, GameEvent,
                                       GameEventHandler, HandlerResult,
                                       runHandlers)
import           Grid                 (Coordinate, Grid)
import           Player               (handlePlayerEvents, playerTile)
import           State                (St (..), enemyPos, playerHealth,
                                       playerPos)
import           UI                   (drawUI)

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
                   , ("Enemy", red `on` black)
                   , ("ui-red", red `on` black) ]

initialState :: St
initialState = St
  { _playerPos = (5, 5)
  , _playerHealth = 3
  , _enemyPos = (14, 12) }

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

run :: IO St
run = do
  eventChan <- newChan
  customMain (mkVty $ mempty Config) (Just eventChan) (app eventChan) initialState
