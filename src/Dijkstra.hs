{-# LANGUAGE OverloadedStrings #-}
-- TODO Document
module Dijkstra
  ( Map
  , Weight (..)
  , solve
  , minimumNeighbor
  ) where

import           Brick.Types         (Widget)
import           Brick.Widgets.Core  (str, withAttr)
import           Control.Arrow
import           Data.Array
import           Data.Maybe
import           Lens.Micro.Platform

import           Grid                (Coordinate, Dimensions, Grid, createGrid,
                                      dim, renderGrid)

type Map = Grid (Maybe Weight)
type Cell = Maybe (Coordinate, Weight)

data Weight = NegInf | Val Integer | PosInf
            deriving (Eq)

instance Show Weight where
  show NegInf  = "-∞"
  show PosInf  = "∞"
  show (Val i) = show i

instance Num Weight where
  negate NegInf  = PosInf
  negate PosInf  = NegInf
  negate (Val i) = Val . negate $ i
  (+) NegInf NegInf   = NegInf
  (+) PosInf PosInf   = PosInf
  (+) NegInf PosInf   = Val 0
  (+) PosInf NegInf   = Val 0
  (+) PosInf (Val _)  = PosInf
  (+) NegInf (Val _)  = NegInf
  (+) (Val _) PosInf  = PosInf
  (+) (Val _) NegInf  = NegInf
  (+) (Val a) (Val b) = Val (a + b)
  (*) NegInf NegInf   = NegInf
  (*) PosInf PosInf   = PosInf
  (*) NegInf PosInf   = Val 0
  (*) PosInf NegInf   = Val 0
  (*) PosInf (Val _)  = PosInf
  (*) NegInf (Val _)  = NegInf
  (*) (Val _) PosInf  = PosInf
  (*) (Val _) NegInf  = NegInf
  (*) (Val a) (Val b) = Val (a * b)
  abs NegInf  = PosInf
  abs PosInf  = PosInf
  abs (Val i) = Val . abs $ i
  signum NegInf  = -1
  signum PosInf  =  1
  signum (Val i) = Val . signum $ i
  fromInteger = Val

instance Ord Weight where
  compare NegInf NegInf   = EQ
  compare PosInf PosInf   = EQ
  compare NegInf _        = LT
  compare _ NegInf        = GT
  compare PosInf _        = GT
  compare _ PosInf        = LT
  compare (Val a) (Val b) = compare a b

solve :: Map -> Map
solve m = if m == nm then m else solve nm
  where nm = calculateWeights m

calculateWeights :: Map -> Map
calculateWeights m = createGrid (dim m) (calculateWeight m)

calculateWeight :: Map -> Coordinate -> Maybe Weight
calculateWeight m c = calcWeight (m ! c) . fmap snd . minimumNeighbor m $ c
  where calcWeight :: Maybe Weight -> Maybe Weight -> Maybe Weight
        calcWeight Nothing mw = Nothing
        calcWeight cw Nothing = cw
        calcWeight (Just cw) (Just mw) = if cw > mw * 2
                                            then Just $ mw + 1
                                            else Just cw

minimumNeighbor :: Map -> Coordinate -> Cell
minimumNeighbor m = foldl minCell Nothing . getNeighbors m
  where minCell :: Cell -> Cell -> Cell
        minCell Nothing b = b
        minCell a Nothing = a
        minCell (Just a@(_, wA))  (Just b@(_, wB)) = if wA < wB then Just a else Just b

getNeighbors :: Map -> Coordinate -> [Cell]
getNeighbors m = map getWeight . neighboringCoordinates m
  where getWeight :: Coordinate -> Cell
        getWeight c = fmap (\w -> (c, w)) (m ! c)

neighboringCoordinates :: Map -> Coordinate -> [Coordinate]
neighboringCoordinates m (x, y) = filter inBounds . map ((+x) *** (+y)) $ deltas
  where (w, h) = dim m
        deltas = [ (-1, -1), ( 0, -1), ( 1, -1)
                 , (-1,  0),           ( 1,  0)
                 , (-1,  1), ( 0,  1), ( 1,  1) ]
        inBounds (x, y) = 0 <= x && x < w && 0 <= y && y < h
