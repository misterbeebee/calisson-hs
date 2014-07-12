-- Algorithm for tiling the diagram with diamond calissons.
module Hexagrid.Tiling where

import           Control.Arrow         (first, (&&&))
import           Core.Function
import           Data.Color
import           Data.Entropy
import qualified Data.IntMap           as M
import qualified Data.IntSet           as IntSet
import           Data.List             (foldl')
import           Data.List             (nub)
import           Data.MapUtil          (mget)
import qualified Debug.Trace           as T
import           Diagrams.Prelude      (blue, green, red)
import           Hexagrid.Grid
import           Hexagrid.Hexacycle
import           Hexagrid.Path
import           Hexagrid.TriangleCell

-- fixme improve these names
mpget k pToC = mget (posToInt k) (positionToColorMap pToC)
mpmget k pToCM = mget (posToInt k) pToCM
mpinsert k v pToC =  M.insert (posToInt k) v (positionToColorMap pToC)
mpminsert k v pToC =  M.insert (posToInt k) v pToC

-- todo: make getColor more intelligent, use diagram context, and get colors in pairs (as calissons)
getColor :: PositionToColor -> (Int,Int) -> Position  -> ColorCode
-- getColor = getHomeColor
getColor pToC _ position = mpget position pToC

applyATiling :: Spec source -> PositionToColor
applyATiling spec = shuffleColors spec $ homeColorization spec

-- Computes a position's color by applying a shuffle function to the "positionColors" scheme
shuffleColors:: Spec source -> PositionToColor -> PositionToColor
shuffleColors spec positionColors =
        let entropy = specPositionEntropy spec in
        map fst (iterate (withEntropyAndRetry (shuffleOnce spec)) (positionColors, entropy))
        !! specShuffles spec

-- warning! this is a "transient" method, (returns an invalid tiling!)
-- caller must do work to maintain invariants!
copyColorFrom :: PositionToColorMap -> (Position, Position) -> PositionToColorMap -> PositionToColorMap
copyColorFrom sourcePToC (srcPos, destPos) destPToC = mpminsert destPos (mpmget srcPos sourcePToC) destPToC


-- A 'shuffle' rotates a unit-hexagon a half-turn, if that would be a valif tiling (flips)
-- Our convention is to use the upper-left corner as the starting-point,
-- so only PointingLeft triangles are valid
-- FIXME! Very few grid positions are valid -- but they are predictable.
-- Build an index at start, and update it (as part of PositionToColor)
shuffleOnce :: Spec source -> Int -> PositionToColor -> Maybe PositionToColor
shuffleOnce spec entropyForCellIndex pToC =
    let theCellPositions = cellPositions spec in
    let theCellOrientations = cellOrientations spec in
    let cellIndex = (entropyForCellIndex `mod` numCells spec) in
    let orientation = (mget cellIndex theCellOrientations) in
    let pos = (mget cellIndex theCellPositions) in
    -- T.trace ("shuffleOnce: " ++ show cellIndex) $
    let cpp = cyclePathPositions orientation pos in
    -- determine which triangles to include in unit-hexagon, from orientation and color of current position
    -- if on a hexagon with 3 diff color, reflect colors across center of hexagon.
    if 
        -- T.trace ("orient: " ++ show orientation) $
        (orientation == PointingLeft) && usableCycle spec cpp pToC
    then
      T.trace ("yep: " ++ show pos) $
      let hexagonPositions = take 6 cpp in
      let hexagonAdjacents = take 6 (zip cpp  (drop 3 cpp)) :: [(Position, Position)] in
      let pToCM = (positionToColorMap pToC) in
      let prevUsableHexagons = (positionToColorUsableHexagons pToC) in
      Just $ PositionToColor
                (foldl' (flip ($)) pToCM (map (copyColorFrom pToCM) hexagonAdjacents))
                (updateUsableHexagons pToC hexagonPositions)
    else
      -- T.trace ("nope: " ++ show pos)
      Nothing

-- warning: cyclic!
usableCycle :: Spec source -> [Position] -> PositionToColor -> Bool
usableCycle spec ps ptoc =
  -- fixme: inefficient list access
  let colorAtPos pos = mpget pos ptoc in
  let colorAt i = colorAtPos (ps !! i) in
  let hexagon = take 6 ps in
  let hexagonColors = map colorAtPos hexagon in
  let colorNub = nub hexagonColors in
  -- T.trace ("hexagon : " ++ show hexagon) $
  all (usable spec) hexagon
      -- Our "upper-left corner, PointingLeft" convention means that only 'red' and 'blue' corners are valid
      && (
         T.trace ("hexagonColors : " ++ show hexagonColors) $
          (hexagonColors !! 0) `elem` [Red, Blue])
      && wellCycledColors hexagonColors

-- adjacent colors are same/diff/... around cycle
wellCycledColors :: Eq a => [a] -> Bool  -- [a] must be cyclic!
-- 5/tail because the first pairing is checked to establish parity.
wellCycledColors (x1:x2:xs) = wellCycledColors' 5 (tail (cycle xs)) (x1 == x2)
--FIXME

wellCycledColors' 0 xs _ = True
wellCycledColors' n (x1:x2:xs) parity =
    ((x1 == x2) == not parity) && wellCycledColors' (n-1) (x2:xs) (not parity)

usable :: Spec source -> Position-> Bool
usable spec p = (Nothing /=) $ M.lookup (posToInt p) (positionToColorMap $ homeColorization spec)
-- fixme: homeBaseColors is just used for its key set, to indicate valid triangle.
