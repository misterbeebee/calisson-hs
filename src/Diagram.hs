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
labelOpacity = 0.2 

-- Needs Renderable Text and Renderable Path, so just hardcode SVG
diagram :: Spec source -> QD SVG
diagram spec =
    DTrace.trace "Building diagram" $
    myframe .
    colorize spec .
    vcatSnug $
    map
       (\(row, (rowStartOrientation, cols)) -> hcatSnug $
                -- add name for indexing
                -- add label as visual aid
                zipWith (\col tri -> named (toName (row, col)) (mkLabel row col <> tri))
                        (fenceposts cols)
                . take cols . drop (fromEnum rowStartOrientation) . cycle
                $ fmap ($ triangle 1 # lineWidth 0) [rotateBy (1/4), rotateBy (-1/4)]
       ) theGridList
  where
    theGridList = gridList spec
    rows = length theGridList

mkLabel row col = scale 0.2 . opacity labelOpacity text $ shows row (',' : show col)

-- fixme
recolor :: ColorCode -> ModifyFn
recolor c cell dia = (getSub cell # fc (colorValue c) # opacity 0.7)  <> dia

-- fixme: awful hack! copy each named subdiagram (which fortunately does not copy the name),
-- and put a colored version `atop` it
colorize :: Spec source -> QDTrans b
colorize spec =
    -- manually lift pToC out of the 'where' clause, or else GHC will recompute it for every position!
    let pToC = positionToColorMap (applyATiling spec) in
    modifyByName (modifyFn pToC) (map intToPos . M.keys $ pToC)
    where
        modifyFn pToC position = recolor (getColor pToC (rows spec, maxCols spec) position)

