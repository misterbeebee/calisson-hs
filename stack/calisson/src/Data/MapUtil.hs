module Data.MapUtil where
import           Data.Maybe                (fromJust)

import qualified Data.IntMap as M
type Map = M.IntMap

foldl1WithKey :: ((Int,v) -> Int -> v -> (Int,v)) -> Map v -> (Int,v)
foldl1WithKey f m =  M.foldlWithKey' f (head . M.toList $ m) m

getValues = map snd . M.toList