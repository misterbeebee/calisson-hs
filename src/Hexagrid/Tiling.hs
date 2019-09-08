{-# LANGUAGE FlexibleInstances #-}
-- Algorithm for tiling the diagram with diamond calissons.
module Hexagrid.Tiling where

import           Control.Arrow         (first, (&&&))
import           Core.Function
import           Core.Math             (l1dist)
import           Data.Color
import           Data.Entropy
import qualified Data.IntMap           as M
import qualified Data.IntSet           as IntSet
import           Data.List             (foldl')
import           Data.List             (nub)
import           Data.Maybe             (isJust, fromJust)
import           Data.Monoid (Sum(Sum), getSum)
import           Data.MapUtil          (Map, foldl1WithKey, getValues)
import qualified Debug.Trace           as T
import           Diagrams.Prelude      (blue, green, red)
import           Data.BinarySearch (Binary, insert, remove, search, get, emptyBinary, binarySize)
import           Hexagrid.Grid
import           Hexagrid.Hexacycle
import           Hexagrid.Path
import           Hexagrid.TriangleCell

-- fixme improve these names
mpget k pToC = mget (posToInt k) (positionToColorMap pToC)
mpmget k posIntMap = mget (posToInt k) posIntMap
mpinsert k v pToC =  M.insert (posToInt k) v (positionToColorMap pToC)
mpminsert k v posIntMap =  M.insert (posToInt k) v posIntMap

type PositionToColorMap = Map ColorCode

data Tiling = Tiling {
        tilingRadius :: Int, -- easier to store than recompute when needed.
        -- colors induced by tiling (the pairs of triangles in a calisson is not explicitly stored)
        positionToColorMap            :: PositionToColorMap,
        -- "upper left corners" of unit-hexagons populated by 3 different tiles, which can be rotated/mirrored
        -- to generate a different valid tiling
        tilingUsableHexagons :: PositionSet
} deriving (Show, Read)


class PositionSetClass a where

instance PositionSetClass IntSet.IntSet where
instance PositionSetClass (Binary Position) where

type PositionSet =
    IntSet.IntSet
    -- Binary Position
    
--fixme typeclass for this switcher and implementations
getImpl intSet bsearch =
    intSet
    -- bseach

asBinary = getImpl undefined id
asIntSet = getImpl id undefined 
   
-- fixme O(n) traversal to get element
-- Use a "Binary (Measure (MinMax Position))" to maintain a tree of these.
-- getNth :: PositionSet -> Int -> Int
getNth tilingUsableHexagons n =
  getImpl
    ((IntSet.toList tilingUsableHexagons) !! n)
    (posToInt $ fromJust $ get (asBinary tilingUsableHexagons) n)
  

insertToPositionSet :: Int -> PositionSet-> PositionSet
insertToPositionSet =
  getImpl
    IntSet.insert
    (insert . intToPos)
    
removeFromPositionSet :: Int -> PositionSet-> PositionSet
removeFromPositionSet =
  getImpl
    IntSet.delete
    (remove . intToPos)
    
emptyPositionSet :: PositionSet
emptyPositionSet  =
  getImpl
    IntSet.empty
    (emptyBinary :: Binary Position)

size = 
  getImpl
    IntSet.size
    (getSum . binarySize)

initialTiling spec =
    let radius = (gridRadius spec) in
    let pToCM = mkCanonicalTiling (mkCornerColors radius ) (orientations spec) in
    (Tiling radius pToCM (mkUsableHexagons (orientations spec) pToCM))

mkUsableHexagons :: Map TriangleOrientation -> PositionToColorMap -> PositionSet
mkUsableHexagons theOrientations pToCM = 
    -- T.trace "mkUsableHexagons"
    foldl' (flip insertToPositionSet) emptyPositionSet $
     filter
        (\posInt ->
            -- T.trace ("mkUsableHexagons: checking usability of hexagon: " ++ show (intToPos posInt)) $
            let orientation = mget posInt theOrientations in 
            isJust (getHexacycleIfUsable orientation (intToPos posInt) pToCM))
        (IntSet.toList $ M.keysSet pToCM)

-- A Hexacycle's position is just the position of its top-left corner.
type HexacyclePosition = Position

posToIntFlattenAndDedup :: [[Position]] -> [Int]
posToIntFlattenAndDedup = IntSet.toList .
    foldl' (\set poss -> foldl' (\set pos -> IntSet.insert (posToInt pos) set)
                                set
                                poss)
           IntSet.empty 

updateUsableHexagons ::  Map TriangleOrientation -> PositionToColorMap -> PositionSet -> [HexacyclePosition] -> PositionSet
updateUsableHexagons theOrientations newColorization oldUsableHexagons changedPositions =
    let getOverlappingHexacycles pos = overlappingHexacycles pos (mpmget pos theOrientations)  newColorization in
    let overlappingHexacylePosInts = posToIntFlattenAndDedup (map getOverlappingHexacycles changedPositions) in
    foldl' go oldUsableHexagons overlappingHexacylePosInts
    where
        go oldUsableHexagons posInt =
          -- T.trace ("updateUsableHexagons: checking usability of hexagon: " ++ show (intToPos posInt)) $
          let update = case getHexacycleIfUsable (mget posInt theOrientations) (intToPos posInt) newColorization of
                         Nothing ->
                             -- T.trace ("remove unusable hexagon: " ++ show (intToPos posInt)) $
                             removeFromPositionSet
                         Just _ ->
                             -- T.trace ("add usable hexagon: " ++ show (intToPos posInt)) $
                             insertToPositionSet
          in
          update posInt oldUsableHexagons


-- validPositions for its keys
overlappingHexacycles :: Position -> TriangleOrientation -> Map a -> [Position]
overlappingHexacycles pos orientation validPositions =
  let paths = case orientation of
              -- paths to hexacycle-positions determined by inspecting a diagram
              PointingLeft ->
                [[],
                [blueToRed, blueToGreen],
                [blueToRed, greenToRed]]
              PointingRight ->
                [[greenToRed, blueToRed, blueToGreen], -- equivalent to bg,br,gr
                [greenToRed],
                [blueToGreen]]
  in
  filter (isValidPosition validPositions) (map (walk' pos) paths) 
   
-- map of positions to position's  colors
-- Map TriangleOrientations is used only for the keys
mkCanonicalTiling :: PositionToColorMap -> Map TriangleOrientation -> PositionToColorMap
mkCanonicalTiling cornerColors positionToDummy = 
   M.fromList .  map (id &&& (getNearestCornerColor cornerColors . intToPos))
   $ M.keys positionToDummy

-- generate map of positions to color of positions' home corner
getNearestCornerColor :: PositionToColorMap -> Position -> ColorCode
getNearestCornerColor cornerColors cell@(Position (row, col)) =
    let fcell = (row,col) in
    snd $ foldl1WithKey
        (\(nearest, ncolor) corner ccolor ->
            -- fixme ugh
            if l1dist fcell (toTuple $ intToPos corner) < l1dist fcell (toTuple $ intToPos nearest)
            then (corner, ccolor)
            else (nearest, ncolor))
        cornerColors

-- map of home postions to colors
mkCornerColors :: Int -> PositionToColorMap
mkCornerColors radius = let s = fromIntegral radius in
  M.fromList . map (first posToInt) $ [
          (Position (-(4*s), 0) , Red)
        , (Position (2*s, -2*s), Green) -- todo, use `rows` instead of `size` ?
        , (Position (2*s, 2*s), Blue)
        ]

getColor :: PositionToColorMap -> Position  -> ColorCode
getColor pToC position = mpmget position pToC

applyATiling :: Spec source -> Maybe Tiling -> Tiling
applyATiling spec mInitialTiling = shuffleColors spec $
        case mInitialTiling of
            Nothing -> initialTiling spec
            Just tiling -> tiling

-- Computes a position's color by applying a shuffle function to the "positionColors" scheme
shuffleColors:: Spec source -> Tiling -> Tiling
shuffleColors spec positionColors =
        let entropy = specPositionEntropy spec in
        map fst (iterate (withEntropyAndRetry (shuffleOnce spec)) (positionColors, entropy))
        !! specShuffles spec

-- warning! this is a "transient" method, (returns an invalid tiling!)
-- caller must do work to maintain invariants!
copyColorFrom :: PositionToColorMap -> (Position, Position) -> PositionToColorMap -> PositionToColorMap
copyColorFrom sourcePToC (srcPos, destPos) destPToC = mpminsert destPos (mpmget srcPos sourcePToC) destPToC


-- A 'shuffle' rotates a unit-hexagon a half-turn, if that would be a valid tiling (flips)
-- Our convention is to use the upper-left corner as the starting-point,
-- so only PointingLeft triangles are valid
-- FIXME! Very few grid positions are valid -- but they are predictable.
-- Build an index at start, and update it (as part of PositionToColor)
shuffleOnce :: Spec source -> Int -> Tiling -> Maybe (Maybe Tiling)
shuffleOnce spec entropyForCellIndex tiling =
    let numUsableHexagons = size (tilingUsableHexagons tiling) in
    case numUsableHexagons of
      0 ->
        T.trace ("ERROR!: No usable hexagons to shuffle colors!") $
        Nothing
      _ ->
        let theOrientations = orientations spec in
        -- todo: pos<->int conversion happens and roundtrip. better to normalize on 'int', for performance
        let hexagonIndex = entropyForCellIndex `mod` numUsableHexagons in
        let posInt = getNth (tilingUsableHexagons tiling) hexagonIndex in
        let pos = intToPos posInt in
        -- T.trace ("shuffleOnce: " ++ show cellIndex) $
        let orientation = mget posInt theOrientations in
        let pToCM = (positionToColorMap tiling) in
        -- rendundant check oh usability, but good for validating computations
        case getHexacycleIfUsable orientation pos pToCM of
          Just hexacycle -> 
           -- T.trace ("yep: " ++ show pos) $
            let hexagonPositions = take 6 hexacycle in
            let hexagonOpposites = take 6 (zip hexacycle (drop 3 hexacycle)) in
            let prevUsableHexagons = (tilingUsableHexagons tiling) in
            let newPToCM = (foldl' (flip ($)) pToCM (map (copyColorFrom pToCM) hexagonOpposites)) in
            Just $ Just $ Tiling (gridRadius spec) newPToCM (updateUsableHexagons theOrientations newPToCM prevUsableHexagons hexagonPositions)
          Nothing ->
              T.trace ("SHOULD NEVER HAPPEN: unsable hexacycle in hexacycle-cache: " ++ show pos) $
              Just Nothing

getHexacycleIfUsable :: TriangleOrientation -> HexacyclePosition -> PositionToColorMap -> Maybe [Position]
getHexacycleIfUsable orientation pos pToC =
      -- determine which triangles to include in unit-hexagon
      -- if on a hexagon with 3 diff color, reflect colors across center of hexagon.
      -- T.trace ("orient: " ++ show orientation) $
      let hexacycle = cyclePathPositions orientation pos in
      if (orientation == PointingLeft) && usableCycle hexacycle pToC
      then Just hexacycle
      else Nothing

-- warning: cyclic!
usableCycle :: [Position] -> PositionToColorMap -> Bool
usableCycle ps ptoc =
  -- fixme: inefficient list access
  let colorAtPos pos = mpmget pos ptoc in
  let colorAt i = colorAtPos (ps !! i) in
  let hexagon = take 6 ps in
  let hexagonColors = map colorAtPos hexagon in
  let colorNub = nub hexagonColors in
  -- T.trace ("hexagon : " ++ show hexagon) $
  all (isValidPosition ptoc) hexagon
      -- Our "upper-left corner, PointingLeft" convention means that only 'red' and 'blue' corners are valid
      && (
         -- T.trace ("hexagonColors : " ++ show hexagonColors) $
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

isValidPosition :: Map a -> Position -> Bool
isValidPosition validPositionSet p = isJust $ M.lookup (posToInt p) validPositionSet


