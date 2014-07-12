{-# LANGUAGE ExistentialQuantification #-}
module Hexagrid.Grid where
-- Calisson's 3-region Hexagon grid
import           Data.Entropy
import qualified Data.IntMap              as M
import           Data.MapUtil(mget, getValues, Map)
import qualified Debug.Trace           as T
import           Hexagrid.TriangleCell

type ColId = Int -- odd signed integer
type RowId = Int -- even signed integer
type RowOrColId = Int -- even signed integer
type Position = (RowId, ColId)

data Spec a = Spec {
        gridRadius                   :: !Int,
        -- fixme doesn't belong in spec
        specShuffles                 :: !Int, -- how many tile-shuffles to make
        specPositionEntropy          :: Entropy Int a,
        rows                         :: !Int,
        maxCols                      :: !Int,
        minCols                      :: !Int,
        gridList                     :: ![(RowId, (TriangleOrientation, Int))],
        cellPositionsWithOrientation :: Map (Position, TriangleOrientation),
        cellPositions                :: Map Position,
        cellOrientations             :: Map TriangleOrientation,
        cellPositionList             :: [Position],
        numCells                     :: !Int
        }

-- compute and save expensive values
mkSpec radius shuffles entropy =
    let rows = mkRows radius in
    let gridList = mkGridList radius rows in
    let cellPositionsWithOrientation = mkCellPositionsWithOrientation gridList in
    let cellPositions = fmap fst cellPositionsWithOrientation in
    Spec radius shuffles entropy rows
        (mkMaxCols radius)
        (mkMinCols radius)
        gridList
        cellPositionsWithOrientation
        cellPositions
        (fmap snd cellPositionsWithOrientation)
        (getValues cellPositions)
        (M.size cellPositions)

mkRows :: Int -> Int
mkRows radius = 4* radius - 1

mkMaxCols :: Int -> Int
mkMaxCols radius = 2 * radius

mkMinCols :: Int -> Int
mkMinCols radius = radius -- fixme

-- coordinates of centers of triangles (`fencepost` is misnomer)
fenceposts :: Int -> [RowOrColId]
fenceposts length =
    fmap
        (\x -> 2 * x - (length - 1))
        [0..length-1]


-- compressed scematic descripting the size of the grid, represented as rows of cells
-- [rowLabel, (parity, numColumns)]
mkGridList :: Int -> Int -> [(RowId, (TriangleOrientation, Int))]
mkGridList radius rows =
  zip (fenceposts rows) $ concat [
  zip (repeat PointingLeft) (fmap (2*) [1..radius]),
  zip (cycle [PointingRight, PointingLeft]) (replicate (2*radius-1) (2*radius)),
  zip (repeat PointingLeft) (fmap (2*) (reverse [1..radius]))
  ]

-- cellLabels, aka positions
-- TODO: switch from 2-D rect coords to 3D (with redundant dimension) triangle coords
mkCellPositionsWithOrientation ::  [(RowId, (TriangleOrientation, Int))] -> Map (Position, TriangleOrientation)
mkCellPositionsWithOrientation gridList = M.fromList (zip [0..] positions)
                where
                 positions = concatMap
                   (\(rowLabel, (rowStartOrientation, numCols)) ->
                     (map (\(col, colLabel) ->
                          let orientation = toEnum $ (fromEnum rowStartOrientation + col) `mod` 2 in
                          ((rowLabel, colLabel), orientation) )
                        (zip [0..] (fenceposts numCols))))  -- (col, colLabel)
                   gridList



-- TODO: make better entropy generator (or interpreter) to jump directly to legit positions
scriptPositionEntropy :: Entropy Int Int
-- 60 is near the center when radius = 5
scriptPositionEntropy = let x = Entropy 7 id (+17) in 
    -- don't use "succ", as that will tend to undo rotations, since adjacent cells root same hexagon
    -- prefer increments relatively prime to the grid size! 
    -- DTrace.trace ("seedPositionEntropy: " ++ show x)
    x

-- simulates a random stream of data
-- fixme make it more interesting
prandPositionEntropy :: Spec source -> Entropy Int Int
prandPositionEntropy spec =
    let x = Entropy 0 id (pseudoRandomCell spec) in
    -- DTrace.trace ("seedPositionEntropy: " ++ show x)
    x


pseudoRandomCell spec curr =
    let theNumCells = numCells spec in
    let cellIndex = (curr * 52237 + 317981) `mod` theNumCells in
    -- DTrace.trace ("pseudoRandomCell: " ++ show (mget cellIndex cellPositions )) $
    cellIndex




data Corner = RedCorner | GreenCorner | BlueCorner -- 0 | 1 | 2
  deriving (Eq, Enum, Show) -- not Ord, because we want to think cyclically!

type UnitVector = (Corner, Corner)
redToGreen  = (RedCorner, GreenCorner)
greenToBlue  = (GreenCorner, BlueCorner)
blueToRed  = (BlueCorner, RedCorner)

-- todo, change to 'data', and adjust math
type SignBit = Int -- -1 | 1

data Orientation = Zero | Unit | Anti
 deriving (Enum, Eq, Show) -- not Ord

toTrit o = case o of
    Zero -> 0
    Unit -> 1
    Anti -> -1

