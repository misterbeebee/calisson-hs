{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Diagram(diagram) where

import           Color                     (ColorCode, cie, colorScale)
import           Control.Arrow             ((&&&))
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
import           Math                      (cartesianProduct, l1dist, (**.),
                                            (/.))

import           Data.Colour.RGBSpace
import           Data.Colour.SRGB.Linear
import           Data.Default
import           Data.List
import           Data.Map                  ((!))
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)

import qualified Debug.Trace               as DTrace

gridSize = 5
rows = 4*gridSize-1
maxCols = 2*gridSize
minCols = gridSize -- fixme

numShuffles = 40 -- 0 for debugging base diagram

-- Needs Renderable Text and Renderable Path, so just hardcode SVG
diagram :: QD SVG
diagram =  DTrace.trace "Building diagram" $
    myframe .
    colorize .
    vcatSnug $
    map
       (\(row, (rowStartOrientation, cols)) -> hcatSnug $
                -- add name for indexing
                -- add label as visual aid
                zipWith (\col tri -> named (toName (row, col)) (mkLabel row col <> tri))
                        (fenceposts cols)
                . take cols . drop (fromEnum rowStartOrientation) . cycle
                $ fmap ($ triangle 1) [rotateBy (1/4), rotateBy (-1/4)]
       ) gridList
  where rows = length gridList


type ColId = Int -- odd signed integer
type RowId = Int -- even signed integer
type RowOrColId = Int -- even signed integer

-- coordinates of centers of triangles (`fencepost` is misnomer)
fenceposts :: Int -> [RowOrColId]
fenceposts length =
    fmap
        (\x -> 2 * x - (length - 1))
        [0..length-1]


-- [rowLabel, (parity, numColumns)]
gridList :: [(RowId, (TriangleOrientation, Int))]
gridList = zip (fenceposts rows) $ concat [
  zip (repeat PointingLeft) (fmap (2*) [1..gridSize]),
  zip (cycle [PointingRight, PointingLeft]) (replicate (2*gridSize-1) (2*gridSize)),
  zip (repeat PointingLeft) (fmap (2*) (reverse [1..gridSize]))
  ]

-- turn down opacity when not debugging
mkLabel row col = scale 0.2 . opacity 0.0 text $ shows row (',' : show col)

-- fixme
recolor :: ColorCode -> ModifyFn
recolor c cell dia = (getSub cell # fc c # opacity 0.7)  <> dia

-- FIXME: verify or fix this!
data TriangleOrientation = PointingLeft | PointingRight
  deriving (Eq, Enum, Show) -- not Ord, because we want to think cyclically!

-- cellLabels, aka positions
-- TODO: switch from 2-D rect coords to 3D (with redundant dimension) triangle coords
cellPositionsWithOrientation :: M.Map Int (Position, TriangleOrientation)
cellPositionsWithOrientation = M.fromList (zip [0..] positions)
                where
                 positions = concatMap
                   (\(rowLabel, (rowStartOrientation, numCols)) ->
                     (map (\(col, colLabel) ->
                          let orientation = toEnum $ (fromEnum rowStartOrientation + col) `mod` 2 in
                          ((rowLabel, colLabel), orientation) )
                        (zip [0..] (fenceposts numCols))))  -- (col, colLabel)
                   gridList -- fixme:- don't duplicate work of  `diagram`

cellPositions = fmap fst cellPositionsWithOrientation
cellOrientations = fmap snd cellPositionsWithOrientation

cellPositionList :: [Position]
cellPositionList = getValues cellPositions

getValues = map (\(k, pos) -> pos) . M.toList
numCells = M.size cellPositions

-- fixme: awful hack! copy each named subdiagram (which fortunately does not copy the name),
-- and put a colored version `atop` it
colorize :: QDTrans b
colorize = modifyByName modifyFn cellPositionList
               where modifyFn name = recolor (getColor (rows, maxCols) name)

-- todo: make getColor more intelligent, use diagram context, and get colors in pairs (as calissons)
getColor :: (Int,Int) -> Position  -> ColorCode
-- getColor = getHomeColor
getColor _ position = mget position theColorization

theColorization = shuffleColors numShuffles homeBaseColors

type Position = (RowId, ColId)
type PositionToColor = M.Map (Position) (ColorCode)

-- Computes a position's color by applying a shuffle function to the "positionColors" scheme
shuffleColors:: Int ->  PositionToColor -> PositionToColor
shuffleColors shuffles positionColors =
    (map fst $ (iterate (withEntropyAndRetry shuffleOnce)) (positionColors, thePositionEntropy)) !! shuffles

-- unsafe lookup
mget :: Ord k => k -> M.Map k v -> v
mget = (fromJust .) . M.lookup

applyThese fs base = foldl (flip ($)) base fs

naturalCycleOrientationFor :: TriangleOrientation -> Orientation
naturalCycleOrientationFor orientation =
    let x = case orientation of
                PointingLeft -> Anti
                PointingRight -> Unit in
    -- DTrace.trace ("naturalCycleOrientationFor: " ++ show orientation ++ " -> " ++ (show x)) $
    x

shuffleOnce :: Int -> PositionToColor -> Maybe PositionToColor
shuffleOnce entropyForCellIndex pToC =
    let cellIndex = entropyForCellIndex `mod` numCells in
    -- DTrace.trace ("shuffleOnce: " ++ show cellIndex) $
    -- only allow positions with properly oriented triangles, so assumptions below are safe
    let triOrient = (mget cellIndex cellOrientations) in
    case triOrient of
     {--
     PointingRight ->
            DTrace.trace "not-desired orientation" $
            Nothing
     --}
      _ ->
          -- DTrace.trace "desired orientation" $
          let colorAt = \pos -> mget pos pToC in
          let pos =  mget cellIndex cellPositions in
          let cpp = cyclePathPositions triOrient  pos in
          -- proper algorithm: determine if on a hexagon with 3 diff colors.
          -- (determine which triangles to include, from orientation and color of current position) if so, reflect colors across hexagon.
          if (usableCycle cpp pToC)
          then
            DTrace.trace ("Position is usable for rotation: " ++ show pos) $
            let hexagon = take 6 (zip cpp  (drop 3 cpp)) :: [(Position, Position)] in
            let oldColor src = mget src pToC in
            let updates = map (\(src, dest) ->
                                       (\d ->
                                            let color = oldColor src in
                                            DTrace.trace (show dest ++ " -> " ++ show src ++ ":" ++ show color) $
                                            M.insert dest color d))
                              hexagon :: [PositionToColor -> PositionToColor]
            in
            Just $ applyThese updates pToC
          else
            DTrace.trace ("Position not usable: " ++ show pos) $
            Nothing


-- warning: cyclic!
usableCycle :: [Position] -> PositionToColor -> Bool
usableCycle ps ptoc =
  -- fixme: inefficient list access
  let colorAt i = (mget (ps !! i) ptoc) in
  let colorAtPos pos = mget pos ptoc in
  let hexagon = (take 6 ps) in
  let hexagonColors = (map colorAtPos hexagon) in
  let colorNub = (nub hexagonColors) in
  -- DTrace.trace ("hexagon : " ++ show  hexagon) $
  (all usable hexagon)
      -- fix me -- should use relative positions to determine if one of the two valid color patterns is present
      -- fixme: this test is too restrictive.
      &&  -- put traces after shortcircuit, in case hexagon has unusable positions.
           (
           --DTrace.trace ("hexagon color: " ++ show  hexagonColors) $
           -- DTrace.trace ("hexagon color nub: size: " ++ show (length colorNub)  {-- ++ "; " ++ show colorNub --} ) $
           -- fixme convert to set and compare
           ((red `elem` colorNub)
           && (green `elem` colorNub)
           && (blue `elem` colorNub)))
      && (colorAt 0) == (colorAt 1)
      && (colorAt 2) == (colorAt 3)
      && (colorAt 4) == (colorAt 5)

usable :: Position-> Bool
usable p =  (Nothing /=) $ M.lookup p homeBaseColors
-- fixme: homeBaseColors is just used for its key set, no indicate valid triangle.




-- TODO move to triangle-math library

-- 0 | 1 | 2
data Corner = Red | Green | Blue
  deriving (Eq, Enum, Show) -- not Ord, because we want to think cyclically!

type UnitVector = (Corner, Corner)
redtoGreen  = (Red, Green)
greenToBlue  = (Green, Blue)
blueToRed  = (Blue, Red)

-- todo, change to 'data', and adjust math
type SignBit = Int -- -1 | 1

data Orientation = Zero | Unit | Anti
 deriving (Enum, Eq, Show) -- not Ord

toTrit o = case o of
    Zero -> 0
    Unit -> 1
    Anti -> -1


type Step = (Corner,Corner) -- direction of vector pointing from one corner to a nother

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
    -- DTrace.trace ("cyclePathPositions: " ++  show (take 6 x)) $
    x



cyclePathSteps :: Step -> Orientation -> [Step]
cyclePathSteps initial orient =
    let steps = iterate (succCycleStep orient) initial in
    -- DTrace.trace ("cyclePathSteps: initial: " ++ show initial ++ "; " ++ show (take 6 steps)) $
    steps

pathPositionsFromSteps :: [Step] -> Position -> [Position]
pathPositionsFromSteps [] _ = [] -- should not happen, input is expected to be a cyclic list
pathPositionsFromSteps (step:steps) initial =
    let next = (move initial step) in    -- fixme this should be a fold
        initial : (pathPositionsFromSteps steps next)

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
            DTrace.trace ("zero vector!" ++ show step)
            id

-- Next step around a hexagon
succCycleStep :: Orientation -> Step -> Step
succCycleStep orient s@(x,y) =
    -- math hack to find "other" Corner
    let enumTotal = (fromEnum Red + fromEnum Blue + fromEnum Green) in
    let z = toEnum $ enumTotal - (fromEnum y + fromEnum x)  in
    -- todo: use a lens-ish thing to make this clean
    -- Star at the diagram to understand why this works :-/
    case (toTrit (stepSign s)) * (toTrit orient) of
        -1 -> (z, y)
        1 -> (x, z)
        0 -> (x, y)


move :: Position -> Step -> Position
move pos@(r,c) step =
    let sign = toTrit (stepSign step) in
    case (stepUnitVector step) of
    -- these formulas are weird, because they are
    -- (a) on the rectangular coordinate grud
    -- (b) they are only valid when moving to an edge-adjacent triangle
    (Red, Green) -> (r + sign*2, c)
    (Green, Blue) -> (r, c+ sign*2)
    (Blue, Red)-> (r - sign*2, c)
    _ -> DTrace.trace ("Error: non-unit vector! " ++ (show step)) (r,c)

data Entropy bit source = Entropy source (source -> bit) (source -> source)
instance Show bit => Show (Entropy bit source) where
    show e@(Entropy ev curr _) = "(Entropy " ++ show (currEntropyBit e)  ++ " _ _)"

currEntropyBit (Entropy ev curr _) = curr ev
nextEntropy (Entropy ev curr next) = Entropy (next ev) curr next


-- Feeds a stream of "random/arbitrary" data to a function (that can fail)
-- retries function (with new entropic value) if it fails
-- todo use state/random/Gen monad
-- fixme stupid name
-- WARNING: can loop forever!
-- Show is just for debugging
withEntropyAndRetry :: Show bit => (bit -> a -> Maybe a) -> (a, Entropy bit e) -> (a, Entropy bit e)
withEntropyAndRetry f (a,e) =
    -- DTrace.trace ("withEntropyAndRetry: ") $
    keepTrying 50 e f a
    where
        keepTrying 0 e f a =
            DTrace.trace ("keepTrying retry-count expired, giving up") $
            (a, nextEntropy e)
        keepTrying triesLeft e f a =
          -- DTrace.trace ("keepTrying: " ++ show e) $
          case (f (currEntropyBit e) a) of
            Nothing ->
                 -- DTrace.trace "retry with entropy!" $
                 keepTrying  (pred triesLeft) (nextEntropy e) f a
            Just fresult -> (fresult, nextEntropy e)

-- thePositionEntropy = prandPositionEntropy
thePositionEntropy = scriptPositionEntropy


-- TODO: make better entropy generator (or interpreter) to jump directly to legit positions
scriptPositionEntropy = let x = Entropy 60 id succ in -- 60 is near the center
    -- DTrace.trace ("seedPositionEntropy: " ++ show x)
    x

-- simulates a random stream of data
-- fixme make it more interesting
prandPositionEntropy :: Entropy Int Int
prandPositionEntropy =
    let x = Entropy 0 id pseudoRandomCell in
    -- DTrace.trace ("seedPositionEntropy: " ++ show x)
    x


pseudoRandomCell curr =
    let cellIndex = (curr * 52237 + 31) `mod` numCells in
    -- DTrace.trace ("pseudoRandomCell: " ++ show (mget cellIndex cellPositions )) $
    cellIndex

-- map of postions to position's homebase colors
homeBaseColors :: PositionToColor
homeBaseColors = M.fromList $ map (id &&& getHomeBaseColor) cellPositionList

-- map of home postions to colors
homeColors :: PositionToColor
homeColors = let s = fromIntegral gridSize in
  M.fromList [
          ((-(4*s), 0) , red)
        , ((2*s, -2*s), green) -- todo, use `rows` instead of `size` ?
        , ((2*s, 2*s), blue)
        ]

getHomeBaseColor :: Position -> ColorCode
getHomeBaseColor cell@(row, col) =
    let fcell = cell in
    snd $ foldl1WithKey
        (\(nearest, ncolor) corner ccolor ->
            -- fixme ugh
            if l1dist fcell corner < l1dist fcell nearest
            then (corner, ccolor)
            else (nearest, ncolor))
        homeColors


foldl1WithKey :: ((k,v) -> k -> v -> (k,v)) -> M.Map k v -> (k,v)
foldl1WithKey f m =  M.foldlWithKey f (head . M.toList $ m) m
