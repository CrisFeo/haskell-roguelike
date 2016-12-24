-- Grid exposes types and functions related to construction and rendering of
-- 2d grids using Brick.
module Grid
  ( Coordinate
  , Dimensions
  , Grid
  , createGrid
  , renderGrid
  , setRange
  ) where

import Brick.Types (Widget)
import Brick.Widgets.Core (hBox, vBox)
import Data.Array
import Lens.Micro.Platform

type Bounds = (Coordinate, Coordinate)
type Coordinate = (Int, Int)
type Dimensions = (Int, Int)
type Grid a = Array Coordinate a

-- Create a new Grid with the specified dimensions. The Grid is populated by
-- the provided function which takes a Coordinate and returns the contents at
-- that location.
createGrid :: Dimensions -> (Coordinate -> a) -> Grid a
createGrid (w, h) f = array domain points
  where domain = ((0, 0), (w-1, h-1))
        points = [((x, y), f (x, y)) | x <- [0..w-1], y <- [0..h-1]]

-- Render a Grid into a hierarchy of Brick widgets which can be rendered into a
-- viewport.
renderGrid :: (a -> Widget ()) -> Grid a -> Widget ()
renderGrid r m = hBox . map (vBox . map r) $ rows
  where ((_, sY), (_, eY)) = bounds m
        rows = divvy (eY - sY + 1) . elems $ m

-- Partition an input list into a list of sublists of the specified length.
-- Depending on the length of the input list the length of the final partition
-- may be shorter than the partition size.
divvy :: Int -> [a] -> [[a]]
divvy _ [] = []
divvy n l = take n l : divvy n (drop n l)

-- Returns a new Grid which is the result of inserting the first Grid into the
-- second at the specifed Coordinate.
setRange :: Coordinate -> Grid a -> Grid a -> Grid a
setRange (x, y) sm m = foldl (&) m . map copyFromSubMap $ coords
  where ((sXm, sYm), (eXm, eYm)) = bounds sm
        coords = [(x, y) | x <- [sXm..eXm], y <- [sYm..eYm]]
        copyFromSubMap c@(xc, yc) = maybe id (ix (x + xc, y + yc) .~) (sm ^? ix c)

