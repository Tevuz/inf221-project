module Game.Util
    ( Float2
    , Time
    , slerp
    , minmax
    , toPair
    , toAngle
    , rot90
    , pairwise
    , segmentLength
    , segmentStart
    , segmentEnd
    ) where

import Linear.V2
import Linear.Metric
import Linear.Vector

import Game.Type

slerp :: Float -> Float2 -> Float2 -> Float2
slerp i p q = p ^* pf + q ^* qf
    where
        theta = acos (dot p q / (norm p * norm q))
        pf    = sin (theta - i * theta) / sin theta
        qf    = sin (        i * theta) / sin theta

minmax :: (Float, Float) -> (Float, Float)
minmax (a, b)
    | a < b     = (a, b)
    | otherwise = (b, a)

toPair :: Float2 -> (Float, Float)
toPair (V2 x y) = (x, y)

toAngle :: Float2 -> Float
toAngle (V2 x y) = atan2 y x * 180 / pi

rot90 :: Float2 -> Float2
rot90 (V2 x y) = V2 (-y) x

pairwise :: [a] -> [(a, a)]
pairwise (a : b : tail) = (a, b) : pairwise tail
pairwise _ = []

segmentLength :: Segment -> Float
segmentLength (Linear (p, q)) = distance p q
segmentLength (Spherical (p, q, c)) =
    let p' = p - c
        q' = q - c
    in  acos (dot p' q' / (norm p' * norm q')) * norm p'

segmentStart :: Segment -> Float2
segmentStart (Linear (p, _)) = p
segmentStart (Spherical (p, _, _)) = p

segmentEnd :: Segment -> Float2
segmentEnd (Linear (_, q)) = q
segmentEnd (Spherical (_, q, _)) = q