{- HLINT ignore "Use newtype instead of data" -}
module Game.Domain.Path
    ( Path(..)
    , createPath
    , pathLength
    , pathStart
    , pathEnd
    ) where

import Linear.V2
import Linear.Metric
import Linear.Vector

import Game.Util
import Game.Type

data Path = Path { segments :: [Segment] }

createPath :: [Float2] -> Path
createPath points = Path (pathInsertCornersInit points)
--path = [Linear (V2 (-360) (-60), V2 360 (-60))]
--path = [Spherical (V2 (-360) (-240), V2 360 (-240), V2 0 (-480))]

pathLength :: Path -> Float
pathLength path = sum $ map segmentLength (segments path)

pathStart :: Path -> Float2
pathStart path = segmentStart $ head (segments path)

pathEnd :: Path -> Float2
pathEnd path = segmentStart $ last (segments path)

pathRadius :: Float
pathRadius = 20

pathInsertCorners :: [Float2] -> [Segment]
pathInsertCorners [] = []
pathInsertCorners [a] = [Linear (a, a)]
pathInsertCorners [a, b] = [Linear (lerp 0.5 a b, b)]
pathInsertCorners (a : b : c : tail)
    | quadrance t < 1e-8 = Linear (am, cm) : pathInsertCorners (b : c : tail)
    | otherwise      = [Linear (am, b0), Spherical (b0, b1, b'), Linear (b1, cm)] ++ pathInsertCorners (b : c : tail)
    where
        ab_ = norm (a - b)
        cb_ = norm (c - b)
        r = min (min pathRadius pathRadius) (0.5 * min ab_ cb_)
        am = lerp 0.5 a b
        cm = lerp 0.5 c b
        t = normalize ((a - b) ^/ ab_) + ((c - b) ^/ cb_)
        b' = b + r / abs (crossZ t ((a - b) ^/ ab_)) *^ t
        b0 = b + project (a - b) (b' - b)
        b1 = b + project (c - b) (b' - b)

pathInsertCornersInit :: [Float2] -> [Segment]
pathInsertCornersInit [] = []
pathInsertCornersInit [a] = [Linear (a, a)]
pathInsertCornersInit [a, b] = [Linear (a, b)]
pathInsertCornersInit (a : b : tail) = Linear (a, lerp 0.5 a b) : pathInsertCorners (a : b: tail)