-- Events exposes types related to Brick event handlers and helpers for
-- dispatching of custom events.
module Events
  ( BrickGameEvent
  , GameEvent (..)
  , GameEventHandler
  , HandlerResult
  , runHandlers
  , sendStep
  ) where

import           Brick.Main             (continue)
import           Brick.Types            (BrickEvent, EventM, Next)
import           Control.Concurrent     (Chan, writeChan)
import           Control.Monad.IO.Class (liftIO)

import           State                  (St)

type BrickGameEvent = BrickEvent () GameEvent
type IntermediateHandlerResult = EventM () St
type HandlerResult = (EventM () (Next St))
type GameEventHandler = Chan GameEvent -> St -> BrickGameEvent -> Either IntermediateHandlerResult HandlerResult

data GameEvent = Step

sendStep :: Chan GameEvent -> St -> IntermediateHandlerResult
sendStep ch st = do
  liftIO $ writeChan ch Step
  return st

runHandlers :: Chan GameEvent -> [GameEventHandler] -> St -> BrickGameEvent -> HandlerResult
runHandlers _ [] st _ = continue st
runHandlers ch (h:hs) st ev = runHandlers' $ h ch st ev
  where runHandlers' :: Either IntermediateHandlerResult HandlerResult -> HandlerResult
        runHandlers' (Left r) = do nst <- r
                                   runHandlers ch hs nst ev
        runHandlers' (Right r) = r

