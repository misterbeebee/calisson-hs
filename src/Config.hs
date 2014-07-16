module Config where

import Core.Math(clampToRange)
import qualified Debug.Trace as T
import Hexagrid.Grid

--- input, defaults, and limits

maxShuffles = 2000
maxRadius = 13

data Input = Input {
    inputRadius :: Int,
    inputShuffles :: Int
}

thePositionEntropy = prandPositionEntropy
-- thePositionEntropy = scriptPositionEntropy

safeRadius :: Int -> Int
safeRadius input =
    let safe = clampToRange (1,maxRadius) input in
    T.trace ("input radius " ++ show input ++ " -> " ++ show safe) 
        safe

safeShuffles :: Int -> Int
safeShuffles input =
    let safe = clampToRange (0,maxShuffles) input in
    T.trace ("input shuffles " ++ show input ++ " -> " ++ show safe)
    safe
