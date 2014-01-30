{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Diagram(diagram) where

import Math((/.), (**.), cartesianProduct)
import Color(colorScale, cie)
import DiagramLib
import Data.List
import Diagrams.Backend.SVG
import Diagrams.Core.Style
import Diagrams.Color.HSV
import Diagrams.Prelude
import Diagrams.TwoD.Shapes(triangle)
import Diagrams.TwoD.Align(snugR)
import Diagrams.TwoD.Transform(rotateBy, rotate)
import Diagrams.TwoD.Types(Turn)
import Diagrams.TwoD.Text(Text)
import Diagrams.TwoD.Combinators

import Data.Colour.RGBSpace
import Data.Colour.SRGB.Linear
import Data.Default

gridSize = 1
rows = 4*gridSize-1
maxCols = 2*gridSize

-- Needs Renderable Text and Renderable Path, so just hardcode SVG
diagram :: QD SVG
diagram = 
   withName (2::Int,3::Int) (recolor white) .
   withName (2::Int,3::Int) (recolor blue) .
   withName (2::Int,3::Int) (recolor orange) .
    frame .
    colorize .
    vcatSnug $   
    map 
       (\(row, (parity, cols)) -> hcatSnug $
                -- add name for indexing
                -- add label as visual aid 
                zipWith (\col tri -> named (toName (row, col)) (mkLabel row col <> tri)) [(0::Int)..]                
                . take cols . drop parity . cycle  -- a row in a large triangle
                $ fmap ($ triangle 1) [rotateBy (1/4), rotateBy (-1/4)]
       ) gridList
  where rows = length gridList


gridList = zip [(0::Int)..] $ concat [
  zip (repeat 0) (fmap (\x -> 2*x) [1..gridSize]),
  zip (cycle [1,0]) (replicate (2*gridSize-1) (2*gridSize)),
  zip (repeat 0) (fmap (\x -> 2*x) (reverse [1..gridSize]))
  ]

mkLabel row col = scale 0.2 . text $ shows row (',' : show col)

-- fixme
recolor :: Colour Double -> ModifyFn
recolor c cell dia = (getSub cell # fc c # opacity 0.5)  <> dia 


cellLabels :: [(Int,Int)]
cellLabels = cartesianProduct [0..rows] [0..maxCols]

-- fixme: awful hack! copy each named subdiagram (which fortunately does not copy the name),
-- and put a colored version `atop` it
colorize :: QDTrans b 
colorize = modifyByName
            (\name sub dia -> colorize1 (rows, maxCols) name (getSub sub) <> dia)
            cellLabels

modifyByName :: (BR2 b, IsName n) => (n -> Subdiagram b R2 Any -> QD b -> QD b) -> [n] -> QDTrans b 
modifyByName modifyFn names d = foldl' (\d name -> withName name (modifyFn name) d) d names
            
--colorize1 _ _ = id
colorize1 :: (BR2 b) => (Int,Int) -> (Int,Int)  -> QDTrans b 
colorize1 = colorizeArb

-- distribute three colors arbitrarily
colorizeArb :: (BR2 b) => (Int,Int) -> (Int,Int)  -> QDTrans b 
colorizeArb (rows,cols) (row, col) =
        let n = ((((2**.col * 3**.row) `mod` 259) **. 10) - 1) `mod` 3 in
                  fc ([red,green,purple] !! n)

