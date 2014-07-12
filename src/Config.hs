module Config where

import Core.Math(clampToRange)
import Data.Maybe(fromMaybe)
import Hexagrid.Grid

--- input, defaults, and limits

data Input = Input {
    inputRadius :: Maybe Int,
    inputShuffles :: Maybe Int
}

-- thePositionEntropy = prandPositionEntropy
thePositionEntropy = scriptPositionEntropy

safeRadius :: Input -> Int
safeRadius = clampToRange (1,5) . fromMaybe 5 . inputRadius

safeShuffles :: Input -> Int
safeShuffles = clampToRange (0,5) . fromMaybe 0 . inputShuffles
