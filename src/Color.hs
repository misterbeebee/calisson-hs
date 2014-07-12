module Color where

import Data.Colour.CIE
import Data.Colour.CIE.Illuminant(d65)

type ColorCode = Colour Double

-- medium lightness CIELAB color
cie = cieLAB d65 50

-- a hacky projection of [0,1]x[0,1] onto the CIELAB color wheel
colorScale m n = cie (200*(m-0.6)) (200*(n-0.5))