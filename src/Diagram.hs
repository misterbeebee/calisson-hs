{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Diagram(diagram) where

import Math((/.), (**.), cartesianProduct)
import Color(colorScale, cie)
import DiagramLib
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
import Data.List
import Data.Map((!))
import qualified Data.Map as M

gridSize = 5
rows = 4*gridSize-1
maxCols = 2*gridSize

-- Needs Renderable Text and Renderable Path, so just hardcode SVG
diagram :: QD SVG
diagram = 
    frame .
    colorize .
    vcatSnug $   
    map 
       (\(row, (parity, cols)) -> hcatSnug $
                -- add name for indexing
                -- add label as visual aid 
                zipWith (\col tri -> named (toName (row, col)) (mkLabel row col <> tri)) 
                        (fenceposts cols)
                . take cols . drop parity . cycle  -- a row in a large triangle
                $ fmap ($ triangle 1) [rotateBy (1/4), rotateBy (-1/4)]
       ) gridList
  where rows = length gridList


-- coordinates of centers of triangles (`fencepost` is misnomer)
fenceposts :: Int -> [Double]
fenceposts length =
    fmap
        (\x -> fromIntegral x - (fromIntegral length - 1) / 2)
        [0..length-1]

gridList = zip (fenceposts rows) $ concat [
  zip (repeat 0) (fmap (\x -> 2*x) [1..gridSize]),
  zip (cycle [1,0]) (replicate (2*gridSize-1) (2*gridSize)),
  zip (repeat 0) (fmap (\x -> 2*x) (reverse [1..gridSize]))
  ]

-- turn down opacity when not debuggin
mkLabel row col = scale 0.2 . opacity 0.2 text $ shows (round $ 2*row) (',' : show (round $ 2*col))

-- fixme
recolor :: Colour Double -> ModifyFn
recolor c cell dia = (getSub cell # fc c # opacity 0.7)  <> dia 


cellLabels :: [(Double,Double)]
cellLabels = cartesianProduct (fenceposts rows) (fenceposts maxCols)

-- fixme: awful hack! copy each named subdiagram (which fortunately does not copy the name),
-- and put a colored version `atop` it
colorize :: QDTrans b 
colorize = modifyByName modifyFn cellLabels
               where modifyFn name = recolor (getColor (rows, maxCols) name)

modifyByName :: (BR2 b, IsName n) => (n -> Subdiagram b R2 Any -> QD b -> QD b) -> [n] -> QDTrans b 
modifyByName modifyFn names d = foldl' (\d name -> withName name (modifyFn name) d) d names
            
getColor :: (Int,Int) -> (Double,Double)  -> Colour Double
getColor = getHomeColor

-- distribute three colors arbitrarily

homeColors :: M.Map (Double, Double) (Colour Double)
homeColors = let s = fromIntegral gridSize in
  M.fromList [
          ((0.5-s*2, 0) , red)
        , ((s, -s), green) -- todo, use `rows instead of `size` ?
        , ((s, s), blue)
        ]

getHomeColor :: (Int,Int) -> (Double,Double) -> Colour Double  
getHomeColor _ cell@(row, col) = 
    let fcell = cell in
    snd $ foldl1WithKey
        (\(nearest, ncolor) corner ccolor ->  
            -- fixme ugh
            if dist fcell corner < dist fcell nearest
            then (corner, ccolor)
            else (nearest, ncolor))
        homeColors


dist a b = norm (a-b)

norm :: (Double, Double) -> Double
norm (x,y) = abs(x) + abs(y)

sqr x = x * x

toF :: (Int, Int) -> (Double, Double)
toF (a,b) = (fromIntegral a, fromIntegral b)

foldl1WithKey :: ((k,v) -> k -> v -> (k,v)) -> M.Map k v -> (k,v)
foldl1WithKey f m =  M.foldlWithKey f (head . M.toList $ m) m