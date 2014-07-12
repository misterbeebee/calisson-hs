-- navigating around a Hexagrid
module Hexagrid.Path where

import qualified Debug.Trace as T
import Hexagrid.Grid
    
type Step = (Corner,Corner) -- direction of vector pointing from one corner to a nother


pathPositionsFromSteps :: [Step] -> Position -> [Position]
pathPositionsFromSteps [] _ = [] -- should not happen, input is expected to be a cyclic list
pathPositionsFromSteps (step:steps) initial =
    let next = move initial step in    -- fixme this should be a fold
        initial : pathPositionsFromSteps steps next

-- if x,y are adjacent in order -> -1. Else, -1
stepSign :: Step -> Orientation
-- abuse mod-3 math here: relative (cyclic) position in Enumeration of Corner
-- corresponds to Enumeration of orientation of vector between Corners.
stepSign (to,from) =  toEnum ((fromEnum from - fromEnum to) `mod` 3)

reflect :: Step -> Step
reflect (from, to) = (to, from)

stepUnitVector :: Step -> UnitVector
stepUnitVector step = normalizer step where
    normalizer = case stepSign step of
       Unit -> id
       Anti -> reflect
       Zero ->
            T.trace ("zero vector!" ++ show step)
            id

move :: Position -> Step -> Position
move pos@(r,c) step =
    let sign = toTrit (stepSign step) in
    case stepUnitVector step of
    -- these formulas are weird, because they are
    -- (a) on the rectangular coordinate grud
    -- (b) they are only valid when moving to an edge-adjacent triangle
    (Red, Green) -> (r + sign*2, c)
    (Green, Blue) -> (r, c+ sign*2)
    (Blue, Red)-> (r - sign*2, c)
    _ -> T.trace ("Error: non-unit vector! " ++ show step) (r,c)
