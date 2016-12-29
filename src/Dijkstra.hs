{-# LANGUAGE OverloadedStrings #-}
-- TODO Document
module Dijkstra
  ( PathMap
  , Weight (..)
  , solve
  , minimumNeighbor
  ) where

import           Control.Arrow
import           Data.Array

import           Grid          (Coordinate, Grid, createGrid, dim)

type PathMap = Grid (Maybe Weight)
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

solve :: PathMap -> PathMap
solve m = if m == nm then m else solve nm
  where nm = calculateWeights m

calculateWeights :: PathMap -> PathMap
calculateWeights m = createGrid (dim m) (calculateWeight m)

calculateWeight :: PathMap -> Coordinate -> Maybe Weight
calculateWeight m c = calcWeight (m ! c) . fmap snd . minimumNeighbor m $ c
  where calcWeight :: Maybe Weight -> Maybe Weight -> Maybe Weight
        calcWeight Nothing _ = Nothing
        calcWeight cw Nothing = cw
        calcWeight (Just cw) (Just mw) = if cw > mw * 2
                                            then Just $ mw + 1
                                            else Just cw

minimumNeighbor :: PathMap -> Coordinate -> Cell
minimumNeighbor m = foldl minCell Nothing . getNeighbors m
  where minCell :: Cell -> Cell -> Cell
        minCell Nothing b = b
        minCell a Nothing = a
        minCell (Just a@(_, wA))  (Just b@(_, wB)) = if wA < wB then Just a else Just b

getNeighbors :: PathMap -> Coordinate -> [Cell]
getNeighbors m = map getWeight . neighboringCoordinates m
  where getWeight :: Coordinate -> Cell
        getWeight c = fmap (\w -> (c, w)) (m ! c)

neighboringCoordinates :: PathMap -> Coordinate -> [Coordinate]
neighboringCoordinates m (x, y) = filter inBounds . map ((+x) *** (+y)) $ deltas
  where (w, h) = dim m
        deltas = [ (-1, -1), ( 0, -1), ( 1, -1)
                 , (-1,  0),           ( 1,  0)
                 , (-1,  1), ( 0,  1), ( 1,  1) ]
        inBounds (cx, cy) = 0 <= cx && cx < w && 0 <= cy && cy < h
