{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Hexagrid.Grid where
-- Calisson's 3-region Hexagon grid
import           Control.Arrow         (first, (&&&), (|||))
import           Data.BinarySearch
import           Data.Bits             (shiftL, shiftR, (.&.))
import           Data.Monoid
import           Data.Color
import           Data.Entropy
import           Data.MinMax
import qualified Data.IntMap           as M
import qualified Data.IntSet           as IntSet
import           Data.MapUtil          (Map, foldl1WithKey, getValues)
import qualified Debug.Trace           as T
import           Hexagrid.TriangleCell

type ColId = Int -- odd signed integer
type RowId = Int -- even signed integer
type RowOrColId = Int -- even signed integer

newtype Position = Position (RowId, ColId)
  deriving (Eq, Show)
    
instance Bounded Position where
       minBound = Position (-99999999999, -99999999999)
       maxBound = Position (99999999999, 99999999999)

instance Ord Position where
    compare m1@(Position (row1, col1)) m2@(Position (row2, col2)) =
        case compare row1 row2 of
               LT -> LT
               GT -> GT
               EQ ->  case compare col1 col2 of
                   LT -> LT
                   GT -> GT
                   EQ -> EQ -- log this case?

      
instance MinMaxable Position where
    
toTuple (Position (x,y)) = (x,y)

  
-- hacky int representation of Position
posToInt (Position (r,c)) =
    let sr = signum r in
    let sc = signum c in
    -- extra multiplication by signum  makes 0 go to 0 sign bit
    let i = (shiftL (abs r) 16) + (shiftL (abs c) 2) + (sr*sr*(1 - sr)) + (shiftR (sc*sc*(1 - sc)) 1) in
    -- T.trace ("posToInt " ++ show (r,c) ++ " -> " ++ show i)
    i

intToPos i =
    let p = Position ((shiftR i 16) * (1 - (i .&. 0x2)), (shiftR (i .&. 0xfffc) 2) * (1 - shiftL (i .&. 0x1) 1)) in
    -- T.trace ("intToPos " ++ show i ++ " -> " ++ show p)
    p

-- unsafe lookup
mget :: Int -> Map v -> v
mget k m =
    case M.lookup k m of
      Nothing -> error ("Lookup failed!!! for key" ++ (show $ intToPos k))
      (Just v) -> v


-- FIXME: this is a total misnomer
data Spec a = Spec {
        gridRadius                         :: !Int,
        -- fixme doesn't belong in spec
        specShuffles                       :: !Int, -- how many tile-shuffles to make
        specPositionEntropy                :: Entropy Int a,
        rows                               :: !Int,
        maxCols                            :: !Int,
        minCols                            :: !Int,
        gridList                           :: ![(RowId, (TriangleOrientation, Int))],
        enumeratedPositionsWithOrientation :: Map (Position, TriangleOrientation), -- key  is [0...numCels]
        orientations                       :: Map TriangleOrientation, -- key is posInt
        numCells                           :: !Int
        -- FIXME: should be in Tiling
        }

-- compute and save expensive values
mkSpec radius shuffles entropy =
    let rows = mkRows radius in
    let gridList = mkGridList radius rows in
    let positionsWithOrientation = mkCellPositionsWithOrientation gridList in
    let
      cellOrientations = M.fromList
        . fmap ((\(pos,orient) -> (posToInt pos, orient)) . snd)
        . M.toList $ positionsWithOrientation in
    -- T.trace ("pwo keys " ++ show positionsWithOrientation) $
    -- T.trace ("orientation keys int" ++ (show $ M.keys (cellOrientations))) $
    -- T.trace ("orientation keys pos " ++ (show $ map intToPos (M.keys (cellOrientations)))) $
    Spec radius shuffles entropy rows
        (mkMaxCols radius)
        (mkMinCols radius)
        gridList
        positionsWithOrientation
        cellOrientations
        (M.size cellOrientations)

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
                          (Position (rowLabel, colLabel), orientation) )
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
prandPositionEntropy :: Entropy Int Int
prandPositionEntropy =
    let x = Entropy 0 id (pseudoRandom) in
    -- DTrace.trace ("seedPositionEntropy: " ++ show x)
    x


pseudoRandom curr = (curr * 52237 + 317981) `mod` (shiftL 2 20)

