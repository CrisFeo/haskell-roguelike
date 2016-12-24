-- Events exposes types related to Brick event handlers and helpers for
-- dispatching of custom events.
module Events
  ( AppEvent
  , GameEvent (..)
  , GameEventHandler
  , HandlerResult
  , runHandlers
  , sendStep
  ) where

import Brick.Main (continue, halt)
import Brick.Types (BrickEvent (VtyEvent), EventM (EventM), Next, Widget)
import Control.Concurrent (Chan, newChan, writeChan)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Lens.Micro.Platform

import Dungeon (dungeonMap, isPassable)
import State (St, playerPos)
import Map (Coordinate)

type AppEvent = BrickEvent () GameEvent
type GameEventHandler = Chan GameEvent -> St -> AppEvent -> Maybe HandlerResult
type HandlerResult = (EventM () (Next St))

data GameEvent = Step

sendStep :: Chan GameEvent -> St -> HandlerResult
sendStep ch st = do
  liftIO $ writeChan ch Step
  continue st

runHandlers :: Chan GameEvent -> [GameEventHandler] -> St -> AppEvent -> Maybe HandlerResult -> HandlerResult
runHandlers ch (h:hs) st ev Nothing = runHandlers ch hs st ev (h ch st ev)
runHandlers _ _ _ _ (Just n)        = n
runHandlers _ [] st _ _             = continue st

