module Math where

import Control.Applicative
import Data.Bifunctor
    
(/.) :: (Integral a, Fractional b) => a -> a -> b
-- (/.) :: Int -> Int -> Double
x /. y = fromIntegral x / fromIntegral y


(**.) :: Int -> Int -> Int
x **. y = round $ fromIntegral x ** fromIntegral y

cartesianProduct as bs = (,) <$> as <*> bs

l1dist a b = l1norm (a-b)

l1norm :: (Double, Double) -> Double
l1norm (x,y) = abs x  + abs y

sqr x = x * x

toF :: Integral a => (a, a) -> (Double, Double)
toF = bimap fromIntegral fromIntegral
