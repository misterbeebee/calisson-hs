module Data.Color where

import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant (d65)
import           Data.Colour.SRGB(sRGB)
import           Diagrams.Prelude(blue, green, red)

type ColorValue = Colour Double

-- More-efficient symbols, for faster comparison
data ColorCode = Red | Green | Blue
    deriving (Eq, Ord, Show, Read)

colorValue Red = sRGB  0.8 0.2 0.2 
colorValue Green = sRGB  0.2 0.8 0.2
colorValue Blue =  sRGB  0.2 0.2 0.8

-- medium lightness CIELAB color
cie = cieLAB d65 50

-- a hacky projection of [0,1]x[0,1] onto the CIELAB color wheel
colorScale m n = cie (200*(m-0.6)) (200*(n-0.5))
