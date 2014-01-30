module Math where

import Control.Applicative
    
(/.) :: (Integral a, Fractional b) => a -> a -> b
-- (/.) :: Int -> Int -> Double
x /. y = fromIntegral x / fromIntegral y


(**.) :: Int -> Int -> Int
x **. y = round $ fromIntegral x ** fromIntegral y

cartesianProduct as bs = (,) <$> as <*> bs