module Math where

import Control.Applicative
import Data.Bifunctor
    
(/.) :: (Integral a, Fractional b) => a -> a -> b
x /. y = fromIntegral x / fromIntegral y


(**.) :: Int -> Int -> Int
x **. y = round $ fromIntegral x ** fromIntegral y

cartesianProduct as bs = (,) <$> as <*> bs

(-.) :: (Num a, Num b) =>  (a,b) -> (a,b) -> (a,b)
(x1,y1) -. (x2,y2) = ((x1-x2), (y1-y2))


l1dist a b = l1norm (a -. b)

l1norm :: Num a => (a, a) -> a
l1norm (x,y) = abs x  + abs y

toF :: Integral a => (a, a) -> (Double, Double)
toF = bimap fromIntegral fromIntegral
