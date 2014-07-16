{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Diagram(diagram) where

import           Control.Arrow             ((&&&))
import           Core.Math                 (cartesianProduct, l1dist, (**.),
                                            (/.))
import           Data.Color                (ColorCode, cie, colorScale, colorValue)
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB.Linear
import           Data.Default
import           Data.Entropy
import           Data.List
import qualified Data.IntMap                  as M
import           Data.MapUtil              (foldl1WithKey)
import           DiagramLib
import           Diagrams.Backend.SVG
import           Diagrams.Color.HSV
import           Diagrams.Core.Style
import           Diagrams.Prelude
import           Diagrams.TwoD.Align       (snugR)
import           Diagrams.TwoD.Combinators
import           Diagrams.TwoD.Shapes      (triangle)
import           Diagrams.TwoD.Text        (Text)
import           Diagrams.TwoD.Transform   (rotate, rotateBy)
import           Hexagrid.Grid
import           Hexagrid.Tiling

import qualified Debug.Trace               as DTrace

-- fixme move to Config
-- turn down opacity when not debugging
labelOpacity = 0.0 

-- Needs Renderable Text and Renderable Path, so just hardcode SVG
diagram :: Spec source -> QD SVG
diagram spec =
    let pToC = positionToColorMap (applyATiling spec) in
    DTrace.trace "Building diagram" $
    myframe .
    vcatSnug $
    map
       (\(row, (rowStartOrientation, cols)) -> hcatSnug $
                -- add name for indexing
                -- add label as visual aid
                zipWith (\col colIndex -> 
                            let pos = Position (row,col) in
                            let rotation = (1/4) * bitToSign (fromEnum rowStartOrientation + colIndex) in
                            let color = colorValue (getColor pToC pos) in
                            mkLabel pos <>
                            triangle 1
                              # named (toName $ toTuple pos)
                              # rotateBy rotation
                              # lineColor color # fillColor color)
                    (fenceposts cols) [0..]
       ) theGridList
  where
    theGridList = gridList spec

mkLabel pos = scale 0.2 . opacity labelOpacity text $ show pos

bitToSign x = case x `mod` 2 of
  0 -> 1
  1 -> -1