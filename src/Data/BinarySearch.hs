module Data.BinarySearch where


import           Data.Monoid (Monoid, Sum, mappend, mempty)


-- from http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers3/
data Binary m = Binary m (Maybe (Binary m, Binary m))


instance (Monoid m) => Monoid (Binary m) where
    mempty = Binary mempty Nothing
    b1@(Binary m1 _) `mappend` b2@(Binary m2 _) = Binary (m1 `mappend` m2) (Just (b1, b2))

data Measure a = Measure  {
    measureSize     :: a,
    measurePosition :: Int
}

search :: Binary (Measure b) -> Int ->  Measure b
search (Binary m Nothing) _ = m
search (Binary _ (Just (c1@(Binary (Measure _ skip) _), c2))) i
    | i' < 0    = search c1 i
    | otherwise = let (Measure s m) = search c2 i' in (Measure s (skip + m))
    where i' = i - skip


newtype Count = Count Int
instance Monoid Count where
    mempty = Count 0
    (Count a) `mappend` (Count b) = Count (a + b)
