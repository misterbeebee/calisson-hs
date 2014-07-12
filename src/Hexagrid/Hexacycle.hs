-- 6-triangle Hexagonal cyclic paths
module Hexagrid.Hexacycle where

import Hexagrid.Grid
import Hexagrid.Path
import Hexagrid.TriangleCell
import qualified Debug.Trace as T

naturalFirstStep :: TriangleOrientation -> Step
naturalFirstStep orient =
    case orient of
        PointingLeft -> (Green, Blue)
        PointingRight -> (Blue, Green)

cyclePathPositions :: TriangleOrientation -> Position -> [Position]
cyclePathPositions triOrient initial =
    let x = pathPositionsFromSteps
                (cyclePathSteps  (naturalFirstStep triOrient)
                (naturalCycleOrientationFor triOrient))  initial in
    -- T.trace ("cyclePathPositions: " ++  show (take 6 x)) $
    x



cyclePathSteps :: Step -> Orientation -> [Step]
cyclePathSteps initial orient =
    let steps = iterate (succCycleStep orient) initial in
    -- T.trace ("cyclePathSteps: initial: " ++ show initial ++ "; " ++ show (take 6 steps)) $
    steps

naturalCycleOrientationFor :: TriangleOrientation -> Orientation
naturalCycleOrientationFor orientation =
    let x = case orientation of
                PointingLeft -> Anti
                PointingRight -> Unit in
    -- DTrace.trace ("naturalCycleOrientationFor: " ++ show orientation ++ " -> " ++ (show x)) $
    x

    
-- Next step around a hexagon
succCycleStep :: Orientation -> Step -> Step
succCycleStep orient s@(x,y) =
    -- math hack to find "other" Corner
    let enumTotal = fromEnum Red + fromEnum Blue + fromEnum Green in
    let z = toEnum $ enumTotal - (fromEnum y + fromEnum x)  in
    -- todo: use a lens-ish thing to make this clean
    -- Star at the diagram to understand why this works :-/
    case toTrit (stepSign s) * toTrit orient of
        -1 -> (z, y)
        1 -> (x, z)
        0 -> (x, y)

