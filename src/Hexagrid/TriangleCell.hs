module Hexagrid.TriangleCell where

data TriangleOrientation = PointingLeft | PointingRight
  deriving (Eq, Enum, Show) -- not Ord, because we want to think cyclically!
