{-# LANGUAGE OverloadedStrings #-}
-- TODO Document
module Enemy
  ( enemyTile
  , handleEnemyEvents
  ) where

import Brick.Types (BrickEvent (AppEvent))
import Lens.Micro.Platform

import Dungeon (Tile (Tile), dungeonMap, isPassable)
import Events (GameEventHandler, GameEvent (Step))
import State (St, enemyPos, playerPos)

enemyTile :: Tile
enemyTile = Tile 'g' "Enemy"

moveEnemy :: St -> St
moveEnemy st = if isPassable dungeonMap newPos
                  then st & enemyPos .~ newPos
                  else st
  where (ex, ey) = st ^. enemyPos
        (px, py) = st ^. playerPos
        (dx, dy) = (signum $ px - ex, signum $ py - ey)
        newPos = (ex + dx, ey + dy)

handleEnemyEvents :: GameEventHandler
handleEnemyEvents _ st (AppEvent Step) = Left . return . moveEnemy $ st
handleEnemyEvents _ st _               = Left . return $ st
