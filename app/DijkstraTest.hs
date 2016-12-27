{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Brick                (App (..), on)
import           Brick.AttrMap        (AttrMap, attrMap)
import           Brick.Main           (defaultMain, neverShowCursor,
                                       resizeOrQuit)
import           Brick.Types          (Widget)
import           Brick.Widgets.Center (center)
import           Brick.Widgets.Core   (str, withAttr)
import           Graphics.Vty         (Color, rgbColor)
import           Lens.Micro.Platform

import           Dijkstra             (Map, Weight (..), solve)
import           Grid                 (Grid, createGrid, renderGrid)

renderMap :: Map -> Widget ()
renderMap = renderGrid renderWeight
  where renderWeight Nothing  = str "   "
        renderWeight (Just w) = str $  " " ++ show w ++ " "

black :: Color
black = rgbColor (0::Int) (0::Int) (0::Int)

white :: Color
white = rgbColor (255::Int) (255::Int) (255::Int)

app :: App Map e ()
app = App { appDraw = \g -> [center . renderMap $ g]
          , appHandleEvent = resizeOrQuit
          , appStartEvent = return
          , appAttrMap = const $ attrMap (white `on` black) []
          , appChooseCursor = neverShowCursor }

initialState :: Map
initialState = solve . foldl setWeight grid $ weights
  where grid = createGrid (10, 10) (const $ Just PosInf)
        setWeight g (c, w) = g & ix c .~ w
        weights = [ ((5, 5), Nothing)
                  , ((5, 4), Nothing)
                  , ((5, 3), Nothing)
                  , ((5, 2), Nothing)
                  , ((5, 1), Nothing)
                  , ((5, 0), Nothing)
                  , ((0, 0), Just (Val 0))
                  , ((3, 4), Just (Val 0)) ]

main :: IO ()
main = do
  _ <- defaultMain app initialState
  return ()
