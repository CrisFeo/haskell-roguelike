{-# LANGUAGE OverloadedStrings #-}
-- TODO Document
module Player
  ( handlePlayerEvents
  , placePlayer
  ) where

import Brick.Types (BrickEvent (VtyEvent))
import Graphics.Vty (Event (EvKey), Key (KChar))
import Lens.Micro.Platform

import Events (GameEventHandler, sendStep)
import Dungeon (dungeonMap, isPassable)
import Map (Coordinate, Map, Tile (Tile), createMap, setRange)
import State (St, playerPos)

playerTile :: Tile
playerTile = Tile '@' "Player"

placePlayer :: Coordinate -> Map -> Map
placePlayer c = setRange c (createMap (1, 1) (const playerTile))

movePlayer :: St -> Coordinate -> St
movePlayer st (dx, dy) = if isPassable dungeonMap newPos
                            then st & playerPos .~ newPos
                            else st
  where (x, y) = st ^. playerPos
        newPos = (x + dx, y + dy)

handlePlayerEvents :: GameEventHandler
handlePlayerEvents ch st (VtyEvent (EvKey (KChar k) [])) =
  case k of
       'h' -> Just . sendStep ch $ movePlayer st (-1,  0)
       'j' -> Just . sendStep ch $ movePlayer st ( 0,  1)
       'k' -> Just . sendStep ch $ movePlayer st ( 0, -1)
       'l' -> Just . sendStep ch $ movePlayer st ( 1,  0)
       'y' -> Just . sendStep ch $ movePlayer st (-1, -1)
       'u' -> Just . sendStep ch $ movePlayer st ( 1, -1)
       'b' -> Just . sendStep ch $ movePlayer st (-1,  1)
       'n' -> Just . sendStep ch $ movePlayer st ( 1,  1)
       _   -> Nothing
handlePlayerEvents _ _ _ = Nothing
