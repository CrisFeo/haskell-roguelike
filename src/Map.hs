{-# LANGUAGE TypeFamilies #-}
-- Map exposes types and functions related to construction and rendering of a
-- 2d grid of Tiles for display using Brick.
module Map
  ( Coordinate
  , Dimensions
  , Map
  , Tile (Tile)
  , createMap
  , renderMap
  , setRange
  ) where

import Brick (withAttr, str)
import Brick.AttrMap (AttrName)
import Brick.Types (Widget)
import Brick.Widgets.Core (hBox, vBox)
import Data.Array
import Lens.Micro.Platform

type Bounds = (Coordinate, Coordinate)
type Coordinate = (Int, Int)
type Dimensions = (Int, Int)
type Map = Array Coordinate Tile

data Tile = Tile { char :: Char
                 , attr :: AttrName }
          deriving (Show, Eq)

-- Create a new Map with the specified dimensions. The Map is populated by the
-- provided function which takes a Coordinate and returns the Tile at that
-- location.
createMap :: Dimensions -> (Coordinate -> Tile) -> Map
createMap (w, h) f = array domain points
  where domain = ((0, 0), (w-1, h-1))
        points = [((x, y), f (x, y)) | x <- [0..w-1], y <- [0..h-1]]

-- Render a Tile as a Brick Widget with the proper display attributes.
renderTile :: Tile -> Widget ()
renderTile (Tile c a) = withAttr a $ str [c]

-- Render a Map of Tiles into a hierarchy of Brick widgets which can be
-- rendered into a viewport.
renderMap :: Map -> Widget ()
renderMap m = hBox . map (vBox . map renderTile) $ rows
  where ((_, sY), (_, eY)) = bounds m
        rows = divvy (eY - sY + 1) . elems $ m

-- Partition an input list into a list of sublists of the specified length.
-- Depending on the length of the input list the length of the final partition
-- may be shorter than the partition size.
divvy :: Int -> [a] -> [[a]]
divvy _ [] = []
divvy n l = take n l : divvy n (drop n l)

-- Retrieve the specified row from the provided map as a list of Tiles.
row :: Map -> Int -> [Tile]
row m i
  | i < sX    = []
  | i > eX    = []
  | otherwise = foldl (\r ci -> m ! (i, ci):r) [] [sY..eY]
  where ((sX, sY), (eX, eY)) = bounds m

-- Returns a new Map where the first Map has been inserted into the second Map
-- at the provided Coordinate.
setRange :: Coordinate -> Map -> Map -> Map
setRange (x, y) sm m = foldl (&) m . map copyFromSubMap $ coords
  where ((sXm, sYm), (eXm, eYm)) = bounds sm
        coords = [(x, y) | x <- [sXm..eXm], y <- [sYm..eYm]]
        copyFromSubMap c@(xc, yc) = maybe id (ix (x + xc, y + yc) .~) (sm ^? ix c)

