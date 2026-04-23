module Game.Type (Float2, Time, Segment(..)) where

import Linear.V2

type Time = Float

type Float2 = V2 Float

data Segment
    = Linear (Float2, Float2)
    | Spherical (Float2, Float2, Float2)