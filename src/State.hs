{-# LANGUAGE TemplateHaskell #-}
-- TODO Document
module State
  ( St (..)
  , enemyPos
  , playerPos
  , playerHealth
  ) where

import           Lens.Micro.Platform

import           Grid                (Coordinate)

data St = St { _playerPos    :: Coordinate
             , _playerHealth :: Int
             , _enemyPos     :: Coordinate } deriving (Show)

makeLenses ''St
