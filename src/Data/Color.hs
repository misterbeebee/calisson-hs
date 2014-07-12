module Data.Color where

import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant (d65)
import           Diagrams.Prelude           (blue, green, red)

type ColorValue = Colour Double

-- More-efficient symbols, for faster comparison
data ColorCode = Red | Green | Blue
    deriving (Eq, Ord, Show)

colorValue Red = red
colorValue Green = green
colorValue Blue = blue

-- medium lightness CIELAB color
cie = cieLAB d65 50

-- a hacky projection of [0,1]x[0,1] onto the CIELAB color wheel
colorScale m n = cie (200*(m-0.6)) (200*(n-0.5))
