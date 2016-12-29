{-# LANGUAGE OverloadedStrings #-}
-- TODO Document
module Enemy
  ( enemyTile
  , handleEnemyEvents
  ) where

import           Brick.Types         (BrickEvent (AppEvent))
import           Data.Maybe
import           Lens.Micro.Platform

import           Dijkstra            (PathMap, Weight (..), minimumNeighbor,
                                      solve)
import           Dungeon             (Tile (Tile), dungeonMap, isPassable)
import           Events              (GameEvent (Step), GameEventHandler)
import           Grid                (Coordinate, createGrid, dim)
import           State               (St, enemyPos, playerHealth, playerPos)

enemyTile :: Tile
enemyTile = Tile 'g' "Enemy"

playerPathMap :: St -> PathMap
playerPathMap st = solve $ pathGrid & ix (st ^. playerPos) .~ Just (Val 0)
  where setWeights :: Coordinate -> Maybe Weight
        setWeights c = if isPassable dungeonMap c then Just PosInf else Nothing
        pathGrid = createGrid (dim dungeonMap) setWeights

getNextPos :: St -> Maybe Coordinate
getNextPos st = fst <$> minimumNeighbor (playerPathMap st) (st ^. enemyPos)

damagePlayer :: St -> St
damagePlayer st = st & playerHealth %~ subtract 1

moveEnemy :: St -> St
moveEnemy st = if nextPos == Just (st ^. playerPos)
                  then damagePlayer st
                  else fromMaybe st $ setEnemyPos <$> nextPos
  where setEnemyPos c = st & enemyPos .~ c
        nextPos = getNextPos st

handleEnemyEvents :: GameEventHandler
handleEnemyEvents _ st (AppEvent Step) = Left . return . moveEnemy $ st
handleEnemyEvents _ st _               = Left . return $ st
