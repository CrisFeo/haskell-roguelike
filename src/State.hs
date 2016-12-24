{-# LANGUAGE TemplateHaskell #-}
-- TODO Document
module State
  ( St (St)
  , enemyPos
  , playerPos
  ) where

import Lens.Micro.Platform

import Grid (Coordinate)

data St = St { _playerPos :: Coordinate
             , _enemyPos :: Coordinate } deriving (Show)

makeLenses ''St
