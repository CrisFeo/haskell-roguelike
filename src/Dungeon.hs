{-# LANGUAGE OverloadedStrings #-}
-- Dungeon is responsible for building/rendering dungeons and querying for
-- various properties of them.
module Dungeon
  ( Tile (Tile)
  , dungeonMap
  , isPassable
  , renderDungeon
  ) where

import Brick.Widgets.Core (withAttr, str)
import Brick.AttrMap (AttrName)
import Brick.Types (Widget)
import Data.Array

import Grid (Coordinate, Dimensions, Grid, createGrid, renderGrid, setRange)

data Tile = Tile Char AttrName
          deriving (Show, Eq)

wall :: Tile
wall = Tile '#' "Wall"

flr :: Tile
flr = Tile '.' "Floor"

renderDungeon :: Grid Tile -> Widget ()
renderDungeon = renderGrid renderTile
  where renderTile (Tile c a) = withAttr a $ str [c]

blankTileGrid :: Dimensions -> Tile -> Grid Tile
blankTileGrid d t = createGrid d (const t)

room :: Dimensions -> Grid Tile
room d@(w, h) = setRange (1, 1) (blankTileGrid (w-2, h-2) flr) (blankTileGrid d wall)

column :: Grid Tile
column = blankTileGrid (2, 2) wall

dungeonMap :: Grid Tile
dungeonMap = foldl (\m (c, sm) -> setRange c sm m) (room mapDim) terrain
  where mapDim = (20, 20)
        terrain = [ ((0 , 0 ), column)
                  , ((18, 0 ), column)
                  , ((9 , 9 ), column)
                  , ((0 , 18), column)
                  , ((18, 18), column)]

isPassable :: Grid Tile -> Coordinate -> Bool
isPassable m c = m ! c == flr
