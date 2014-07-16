module Data.MinMax where

import           Data.Monoid (Monoid, Sum, mappend, mempty)


newtype MinMax a = MinMax (a, a)
    deriving (Eq, Show, Bounded)

class (Bounded n, Eq n, Ord n, Show n) => MinMaxable n

-- (min, max)
instance (Show a, Bounded a, Ord a) => Monoid (MinMax a) where
    mempty = error "MinMax has no mempty" undefined  -- this  should not be needed!
    mappend m1@(MinMax (min1, max1)) m2@(MinMax (min2, max2)) =
        if (max1 > min2) then
            error ("invalid MinMax combination: not adjacent: " ++ show m1 ++ ", " ++ show m2) $
            undefined
        else
            MinMax (min min1 min2, max max1 max2)

    
instance (Ord a) => Ord (MinMax a) where
    -- this is sketchy. It's only valid in some cases
    