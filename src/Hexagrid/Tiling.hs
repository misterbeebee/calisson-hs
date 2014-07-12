-- Algorithm for tiling the diagram with diamond calissons.
module Hexagrid.Tiling where

import           Control.Arrow      (first, (&&&))
import           Core.Function
import           Core.Math
import           Data.Bits          (shiftL, shiftR, (.&.))
import           Data.Color
import           Data.Entropy
import qualified Data.IntMap        as M
import           Data.List          (foldl')
import           Data.List          (nub)
import           Data.MapUtil
import qualified Debug.Trace        as T
import           Diagrams.Prelude   (blue, green, red)
import           Hexagrid.Grid
import           Hexagrid.Hexacycle
import           Hexagrid.Path
import           Hexagrid.TriangleCell

type PositionToColor = Map ColorCode

-- hacky int representation
posToInt (r,c) = (shiftL (abs r) 16) + (shiftL (abs c) 2) + ((1 - signum r)) + (shiftR (1 - signum c) 1)
intToPos i = ((shiftR i 16) * (1 - (i .&. 0x2)),
              (shiftR (i .&. 0xfffb) 2) * (1 - shiftL (i .&. 0x1) 1))

mpget = mget . posToInt
mpinsert =  M.insert . posToInt

-- todo: make getColor more intelligent, use diagram context, and get colors in pairs (as calissons)
getColor :: PositionToColor -> (Int,Int) -> Position  -> ColorCode
-- getColor = getHomeColor
getColor pToC _ position = mpget position pToC

theColorization :: Spec source -> PositionToColor
theColorization spec = shuffleColors spec $ homeBaseColors spec

-- Computes a position's color by applying a shuffle function to the "positionColors" scheme
shuffleColors:: Spec source -> PositionToColor -> PositionToColor
shuffleColors spec positionColors =
        let entropy = specPositionEntropy spec in
        map fst (iterate (withEntropyAndRetry (shuffleOnce spec)) (positionColors, entropy))
        !! specShuffles spec

-- map of postions to position's homebase colors
homeBaseColors :: Spec source -> PositionToColor
homeBaseColors spec = M.fromList $ map (posToInt &&& getHomeBaseColor spec) $ cellPositionList spec

-- map of home postions to colors
homeColors :: Spec source -> PositionToColor
homeColors spec = let s = fromIntegral (gridRadius spec) in
  M.fromList . map (first posToInt) $ [
          ((-(4*s), 0) , Red)
        , ((2*s, -2*s), Green) -- todo, use `rows` instead of `size` ?
        , ((2*s, 2*s), Blue)
        ]

getHomeBaseColor :: Spec source -> Position -> ColorCode
getHomeBaseColor spec cell@(row, col) =
    let fcell = cell in
    snd $ foldl1WithKey
        (\(nearest, ncolor) corner ccolor ->
            -- fixme ugh
            if l1dist fcell (intToPos corner) < l1dist fcell (intToPos nearest)
            then (corner, ccolor)
            else (nearest, ncolor))
        (homeColors spec)

copyColorFrom :: PositionToColor -> (Position, Position) -> PositionToColor -> PositionToColor
copyColorFrom sourcePToC (srcPos, destPos) destPToC = mpinsert destPos (mpget srcPos sourcePToC) destPToC

shuffleOnce :: Spec source -> Int -> PositionToColor -> Maybe PositionToColor
shuffleOnce spec entropyForCellIndex pToC =
    let theCellPositions = cellPositions spec in
    let theCellOrientations = cellOrientations spec in
    -- A 'shuffle' rotates a unit-hexagon a half-turn, if that would be a valif tiling (flips)
    -- Our convention is to use the upper-left corner as the starting-point, 
    -- so only PointingLeft triangles are valid
    let cellIndex = (entropyForCellIndex `mod` numCells spec) in
    let orientation = (mget cellIndex theCellOrientations) in
    let pos = (mget cellIndex theCellPositions) in
    -- T.trace ("shuffleOnce: " ++ show cellIndex) $
    -- T.trace "desired orientation" $
    let cpp = cyclePathPositions orientation pos in
    -- determine which triangles to include in unit-hexagon, from orientation and color of current position 
    -- if on a hexagon with 3 diff color, reflect colors across center of hexagon.
    if (orientation == PointingLeft) && usableCycle spec cpp pToC
    then
      T.trace ("yep: " ++ show pos) $
      let hexagon = take 6 (zip cpp  (drop 3 cpp)) :: [(Position, Position)] in
      Just $ foldl' (flip ($)) pToC (map (copyColorFrom pToC) hexagon) 
    else
       T.trace ("nope: " ++ show pos)
      Nothing

-- warning: cyclic!
usableCycle :: Spec source -> [Position] -> PositionToColor -> Bool
usableCycle spec ps ptoc =
  -- fixme: inefficient list access
  let colorAtPos pos = mget (posToInt pos) ptoc in
  let colorAt i = colorAtPos (ps !! i) in
  let hexagon = take 6 ps in
  let hexagonColors = map colorAtPos hexagon in
  let colorNub = nub hexagonColors in
  -- DTrace.trace ("hexagon : " ++ show  hexagon) $
  all (usable spec) hexagon
      -- Our "upper-left corner, PointingLeft" convention means that only 'red' and 'blue' corners are valid 
      && (hexagonColors !! 0) `elem` [Red, Blue]
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
usable spec p = (Nothing /=) $ M.lookup (posToInt p) $ homeBaseColors spec
-- fixme: homeBaseColors is just used for its key set, no indicate valid triangle.
