module Data.MapUtil where
import           Data.Maybe                (fromJust)

import qualified Data.IntMap as M
type Map = M.IntMap

foldl1WithKey :: ((Int,v) -> Int -> v -> (Int,v)) -> Map v -> (Int,v)
foldl1WithKey f m =  M.foldlWithKey' f (head . M.toList $ m) m

-- unsafe lookup
mget :: Int -> Map v -> v
mget = (fromJust .) . M.lookup

getValues = map snd . M.toList