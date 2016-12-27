{-# LANGUAGE OverloadedStrings #-}
-- TODO Document
module Enemy
  ( enemyTile
  , handleEnemyEvents
  ) where

import           Brick.Types         (BrickEvent (AppEvent))
import           Data.Array
import           Data.List
import           Data.Maybe
import           Lens.Micro.Platform

import           Dijkstra            (Weight (..), minimumNeighbor, solve)
import           Dungeon             (Tile (Tile), dungeonMap, isPassable)
import           Events              (GameEvent (Step), GameEventHandler)
import           Grid                (createGrid, dim)
import           State               (St, enemyPos, playerPos)

enemyTile :: Tile
enemyTile = Tile 'g' "Enemy"

moveEnemy :: St -> St
moveEnemy st = fromMaybe st $ (\c -> st & enemyPos .~ c) . fst <$> minNeighbor
  where setInitialWeight c = if isPassable dungeonMap c then Just PosInf else Nothing
        pathGrid = createGrid (dim dungeonMap) setInitialWeight
        playerGrid = solve $ pathGrid & ix (st ^. playerPos) .~ Just (Val 0)
        minNeighbor = minimumNeighbor playerGrid (st ^. enemyPos)

handleEnemyEvents :: GameEventHandler
handleEnemyEvents _ st (AppEvent Step) = Left . return . moveEnemy $ st
handleEnemyEvents _ st _               = Left . return $ st
