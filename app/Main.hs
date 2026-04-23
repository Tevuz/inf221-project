{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Graphics.Gloss

import Linear.V2
import Linear.Metric
import Linear.Vector

type Float2 = V2 Float
type Path = [Float2]

type Time = Float

main :: IO ()
main = animate window background frame

window :: Display
window = InWindow "Tower Defence" (800, 600) (100, 100)

background :: Color
background = black

frame :: Time -> Picture
frame time = Pictures $
       [Color yellow $ drawPath path]
    ++ map (drawTower time) towers
    ++ map (drawEnemy time) enemies

square :: Float -> Float
square x = x * x

pathRadius :: Float
pathRadius = 20

pathPoints :: Main.Path
--pathPoints = map (uncurry V2) [ (-360, -60), (-120, -60), (-60, 60), (60, 60), (120, -60), (360, -60) ]
pathPoints = map (uncurry V2) [ (-360, -60), (-150, -60), (-90, 60), (90, 60), (150, -60), (360, -60), (360, 60), (150, 60), (90, -60), (-90, -60), (-150, 60), (-360, 60) ]
--pathPoints = map (uncurry V2)  [ (-300, -60), (-120, -60), (0, 120), (120, -60), (300, -60) ]

data Segment
    = Linear (Float2, Float2)
    | Spherical (Float2, Float2, Float2)

pathInsertCorners :: Main.Path -> [Segment]
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

pathInsertCornersInit :: Main.Path -> [Segment]
pathInsertCornersInit [] = []
pathInsertCornersInit [a] = [Linear (a, a)]
pathInsertCornersInit [a, b] = [Linear (a, b)]
pathInsertCornersInit (a : b : tail) = Linear (a, lerp 0.5 a b) : pathInsertCorners (a : b: tail)

path :: [Segment]
path = pathInsertCornersInit pathPoints
--path = [Linear (V2 (-360) (-60), V2 360 (-60))]
--path = [Spherical (V2 (-360) (-240), V2 360 (-240), V2 0 (-480))]

segmentLength :: Segment -> Float
segmentLength (Linear (p, q)) = distance p q
segmentLength (Spherical (p, q, c)) =
    let p' = p - c
        q' = q - c
    in  acos (dot p' q' / (norm p' * norm q')) * norm p'

pathLength :: Float
pathLength = sum $ map segmentLength path

segmentStart :: Segment -> Float2
segmentStart (Linear (p, _)) = p
segmentStart (Spherical (p, _, _)) = p

segmentEnd :: Segment -> Float2
segmentEnd (Linear (_, q)) = q
segmentEnd (Spherical (_, q, _)) = q

pathIntersect :: Point -> Float -> [Segment] -> Float -> [Float]
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

calcArea :: Point -> Float -> [(Float, Float)]
calcArea (x, y) range = pairwise
    (  [0 | distance (V2 x y) (segmentStart $ head path) < range]
    ++ pathIntersect (x, y) range path 0
    ++ [1 | distance (V2 x y) (segmentEnd $ last path) < range])

data Tower = Tower { position :: Point, range :: Float, area :: [(Float, Float)] }
createTower :: Point -> Float -> Tower
createTower pos range = Tower pos range (calcArea pos range)

towers :: [Tower]
towers =
    [ createTower (-180,  0) 80
    , createTower (   0,  0) 80
    , createTower ( 180,  0) 80
    ]

areaCoverage :: [(Float, Float, Tower)]
areaCoverage = concatMap (\tower -> map (\(a, b) -> (a, b, tower)) (area tower)) towers

data Enemy = Enemy { progress :: Float }
enemies :: [Enemy]
enemies =
    [ Enemy { progress = -0.0 }
    , Enemy { progress = -60.0 }
    , Enemy { progress = -120.0 }
    , Enemy { progress = -180.0 }
    , Enemy { progress = -240.0 }
    ]

drawTower :: Time -> Tower -> Picture
drawTower time tower =
    Pictures
    [   uncurry translate (position tower) $ Pictures
        [   color white $ Line [ (-20, -20), (20, -20), (20, 20), (-20, 20), (-20, -20) ]
        ,   color white $ Circle 20
        ,   color cyan $ Circle (range tower)
        ,   color white $ Line [ (0, 0), (sin (time * 3.1415 / 60.0) * 20, cos (time * 3.1415 / 60.0) * 20) ]
        ]
    ,   color green $ drawPointOnPath (concatMap (\(x, y) -> [x, y]) $ area tower)
    ]

drawPointOnPath :: [Float] -> Picture
drawPointOnPath [] = Blank
drawPointOnPath (t : tail)
    | Just pos <- position = Pictures [uncurry translate (toPair pos) $ Circle 5, drawPointOnPath tail]
    | otherwise = Blank
    where
        position = pathPoint path t

segmentPoint :: Segment -> Float -> Float2
segmentPoint (Linear (p, q)) t = lerp t p q
segmentPoint (Spherical (p, q, c)) t = c + slerp t (p - c) (q - c)

pathPoint :: [Segment] -> Float -> Maybe Float2
pathPoint [] _ = Nothing
pathPoint (segment : tail) t
    | t < 0    = Nothing
    | t <= l    = Just $ segmentPoint segment (t / l)
    | otherwise = pathPoint tail (t - l)
    where l = segmentLength segment

drawEnemy :: Time -> Enemy -> Picture
drawEnemy time Enemy { progress }
    | Just pos <- position = uncurry translate (toPair pos) $ color col $ Circle 16
    | otherwise = Blank
    where
        t = time * 60.0 + progress
        position = pathPoint path $ t
        col = if any (\(a, b, _) -> a < t && t < b) areaCoverage then green else red

drawPath :: [Segment] -> Picture
drawPath [] = Blank
drawPath (Linear (p, q) : tail) = Pictures
    [   Line [toPair p, toPair q]
    ,   uncurry translate (toPair p) $ color orange $ Circle 2
    ,   uncurry translate (toPair q) $ color orange $ Circle 2
    ,   drawPath tail
    ]
drawPath (Spherical (p, q, c) : tail) = Pictures
    [   uncurry translate (toPair c) $ Arc pa qa (distance p c)
    ,   uncurry translate (toPair p) $ color orange $ Circle 2
    ,   uncurry translate (toPair q) $ color orange $ Circle 2
    ,   drawPath tail
    ]
    where
        (pa, qa) = minmax (toAngle (p - c), toAngle (q - c))

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