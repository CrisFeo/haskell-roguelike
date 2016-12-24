{-# LANGUAGE OverloadedStrings #-}
-- TODO Document
module Enemy
  ( handleEnemyEvents
  , placeEnemy
  ) where

import Brick.Main (continue)
import Brick.Types (BrickEvent (AppEvent))
import Lens.Micro.Platform

import Dungeon (Tile (Tile), dungeonMap, isPassable)
import Events (GameEventHandler, GameEvent (Step))
import Grid (Coordinate, Grid, createGrid, setRange)
import State (St, enemyPos, playerPos)

enemyTile :: Tile
enemyTile = Tile 'g' "Enemy"

placeEnemy :: Coordinate -> Grid Tile -> Grid Tile
placeEnemy c = setRange c (createGrid (1, 1) (const enemyTile))

moveEnemy :: St -> St
moveEnemy st = if isPassable dungeonMap newPos
                  then st & enemyPos .~ newPos
                  else st
  where (ex, ey) = st ^. enemyPos
        (px, py) = st ^. playerPos
        (dx, dy) = (signum $ px - ex, signum $ py - ey)
        newPos = (ex + dx, ey + dy)

handleEnemyEvents :: GameEventHandler
handleEnemyEvents ch st (AppEvent Step) = Left . return . moveEnemy $ st
handleEnemyEvents _ st _      = Left . return $ st
