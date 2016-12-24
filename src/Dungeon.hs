{-# LANGUAGE OverloadedStrings #-}
-- Dungeon is responsible for building dungeon maps and querying for various
-- properties of dungeons.
module Dungeon
  ( dungeonMap
  , isPassable
  ) where

import Data.Array

import Map (Coordinate, Dimensions, Map, Tile (Tile), createMap, setRange)

wall :: Tile
wall = Tile '#' "Wall"

flr :: Tile
flr = Tile '.' "Floor"

blankMap :: Dimensions -> Tile -> Map
blankMap d t = createMap d (const t)

room :: Dimensions -> Map
room d@(w, h) = setRange (1, 1) (blankMap (w-2, h-2) flr) (blankMap d wall)

column :: Map
column = blankMap (2, 2) wall

dungeonMap :: Map
dungeonMap = foldl (\m (c, sm) -> setRange c sm m) (room mapDim) terrain
  where mapDim = (20, 20)
        terrain = [ ((0 , 0 ), column)
                  , ((18, 0 ), column)
                  , ((9 , 9 ), column)
                  , ((0 , 18), column)
                  , ((18, 18), column)]

isPassable :: Map -> Coordinate -> Bool
isPassable m c = m ! c == flr
