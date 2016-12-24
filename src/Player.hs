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
import Dungeon (Tile (Tile), dungeonMap, isPassable)
import Grid (Coordinate, Grid, createGrid, setRange)
import State (St, playerPos)

playerTile :: Tile
playerTile = Tile '@' "Player"

placePlayer :: Coordinate -> Grid Tile -> Grid Tile
placePlayer c = setRange c (createGrid (1, 1) (const playerTile))

movePlayer :: St -> Coordinate -> St
movePlayer st (dx, dy) = if isPassable dungeonMap newPos
                            then st & playerPos .~ newPos
                            else st
  where (x, y) = st ^. playerPos
        newPos = (x + dx, y + dy)

handlePlayerEvents :: GameEventHandler
handlePlayerEvents ch st (VtyEvent (EvKey (KChar k) [])) =
  case k of
       'h' -> Left . sendStep ch . movePlayer st $ (-1,  0)
       'j' -> Left . sendStep ch . movePlayer st $ ( 0,  1)
       'k' -> Left . sendStep ch . movePlayer st $ ( 0, -1)
       'l' -> Left . sendStep ch . movePlayer st $ ( 1,  0)
       'y' -> Left . sendStep ch . movePlayer st $ (-1, -1)
       'u' -> Left . sendStep ch . movePlayer st $ ( 1, -1)
       'b' -> Left . sendStep ch . movePlayer st $ (-1,  1)
       'n' -> Left . sendStep ch . movePlayer st $ ( 1,  1)
       _   -> Left . return $ st
handlePlayerEvents _ st _ = Left . return $ st
