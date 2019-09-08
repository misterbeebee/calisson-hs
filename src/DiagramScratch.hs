{-# LANGUAGE FlexibleContexts #-}
module DiagramScratch where
-- scratch functions not used in main program

import Diagram
import Core.Math((/.), (**.))
import Data.Color(colorScale)

import Diagrams.Prelude


-- apply a fill-color gradient across a list of diagrams
colorizeGradient :: (Int,Int) -> (Int,Int) -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any 
colorizeGradient  (rows,cols) (row,col) = 
       recommendFillColor (colorScale (row /. rows) (col /. cols))

colorOrigin :: (Renderable (Path V2 Double) b, Backend b V2 Double, Monoid' m) => Double ->   QDiagram b V2 Double m -> QDiagram b V2 Double m
colorOrigin x = showOrigin' (with & oColor .~ colorScale x 0 & oScale .~ 0.01)
