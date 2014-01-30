{-# LANGUAGE FlexibleContexts #-}
module DiagramScratch where
-- scratch functions not used in main program

import Diagram
import Math((/.), (**.))
import Color(colorScale)

import Diagrams.Prelude


-- apply a fill-color gradient across a list of diagrams
colorizeGradient :: (Int,Int) -> (Int,Int) -> QDiagram b R2 Any -> QDiagram b R2 Any
colorizeGradient  (rows,cols) (row,col) = 
       recommendFillColor (colorScale (row /. rows) (col /. cols))

colorOrigin :: (Renderable (Path R2) b, Backend b R2, Monoid' m) => Double ->   QDiagram b R2 m -> QDiagram b R2 m
colorOrigin n = showOrigin' (with & oColor .~ colorScale n 0 & oScale .~ 0.01)
