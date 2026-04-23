{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Game.Domain.Tower (Tower(..), createTower) where

import Linear.V2
import Linear.Metric
import Linear.Vector

import Game.Util
import Game.Type
import Game.Domain.Path

data Tower = Tower { position :: (Float, Float), range :: Float, area :: [(Float, Float)] }
createTower :: (Float, Float) -> Float -> Path -> Tower
createTower pos range path = Tower pos range (calcArea path pos range)

calcArea :: Path -> (Float, Float) -> Float -> [(Float, Float)]
calcArea path (x, y) range = pairwise
    (  [0 | distance (V2 x y) (pathStart path) < range]
    ++ pathIntersect (x, y) range (segments path) 0
    ++ [1 | distance (V2 x y) (pathEnd path) < range])

pathIntersect :: (Float, Float) -> Float -> [Segment] -> Float -> [Float]
pathIntersect _ _ [] _ = []
pathIntersect (x, y) r (Linear (p, q) : tail) t
    | d <= 0    = pathIntersect (x, y) r tail t'
    | otherwise =
        let s = sqrt d
            p = (-b - s) / (2 * a)
            q = (-b + s) / (2 * a)
        in [t + l * p | 0 < p && p < 1] ++ [t + l * q | 0 < q && q < 1] ++ pathIntersect (x, y) r tail t'
    where
        t' = t + segmentLength (Linear (p, q))
        pq = q - p
        cp = p - V2 x y
        l = norm pq
        a = dot pq pq
        b = 2 * dot pq cp
        c = dot cp cp - r * r
        d = b * b - 4 * a * c
pathIntersect (x, y) r0 (Spherical (p, q, c) : tail) t =
    let t' = t + segmentLength (Spherical (p, q, c))
        v = V2 x y - c
        d = norm v
        r1 = distance p c
    in  if d > r0 + r1 || d < abs (r1 - r0) || d == 0 && r0 == r1
        then pathIntersect (x, y) r0 tail t'
    else
    let a = (r1 * r1 - r0 * r0 + d * d) / (2 * d)
        h2 = r1 * r1 - a * a
    in if h2 < 0
        then pathIntersect (x, y) r0 tail t'
    else
    let h = sqrt h2

        m = v ^* (a / d)
        mt = rot90 v ^* (h / d)

        pa = toAngle (p - c)
        qa = toAngle (q - c)

        at = (toAngle (m + mt) - pa) / (qa - pa)
        bt = (toAngle (m - mt) - pa) / (qa - pa)
    in [t + (t' - t) * at | 0 < at && at < 1]
    ++ [t + (t' - t) * bt | 0 < bt && bt < 1]
    ++ pathIntersect (x, y) r0 tail t'

--areaCoverage :: [(Float, Float, Tower)]
--areaCoverage = concatMap (\tower -> map (\(a, b) -> (a, b, tower)) (area tower)) towers