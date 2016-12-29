{-# LANGUAGE OverloadedStrings #-}
module UI
  (drawUI) where

import           Brick.Types          (BrickEvent (VtyEvent), Padding (..),
                                       Widget)
import           Brick.Widgets.Border (borderWithLabel)
import           Brick.Widgets.Center (center, hCenter)
import           Brick.Widgets.Core   (hBox, hLimit, padLeft, str, vBox,
                                       withAttr)
import           Lens.Micro.Platform

import           Dungeon              (Tile, dungeonMap, renderDungeon)
import           Enemy                (enemyTile, handleEnemyEvents)
import           Grid                 (Coordinate, Grid)
import           Player               (handlePlayerEvents, playerTile)
import           State                (St (..), enemyPos, playerHealth,
                                       playerPos)

place :: Tile -> Coordinate -> Grid Tile -> Grid Tile
place t c g = g & ix c .~ t

drawDungeon :: St -> Widget ()
drawDungeon st = renderDungeon . placePlayer . placeEnemy $ dungeonMap
  where placeEnemy = place enemyTile (st ^. enemyPos)
        placePlayer = place playerTile (st ^. playerPos)

drawStatus :: St -> Widget ()
drawStatus st = padLeft (Pad 2) . borderWithLabel (str "Status") . hLimit 10 . vBox $ lines
  where lines = [ str "James"
                , hCenter . withAttr "ui-red" . str $ replicate (st ^. playerHealth) '*' ]

drawUI :: St -> [Widget ()]
drawUI st =  [center . hBox $ [drawDungeon st, drawStatus st]]
