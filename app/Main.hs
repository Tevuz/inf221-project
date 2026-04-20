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
pathPoints = map (uncurry V2) [ (-360, -60), (-120, -60), (-60, 60), (60, 60), (120, -60), (360, -60) ]
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

segmentLength :: Segment -> Float
segmentLength (Linear (p, q)) = distance p q
segmentLength (Spherical (p, q, c)) =
    let p' = p - c
        q' = q - c
    in  acos (dot p' q' / (norm p' * norm q')) * norm p'

pathLength :: Float
pathLength = sum $ map segmentLength path

data Tower = Tower { position :: Point }
towers :: [Tower]
towers =
    [ Tower (0,  0)
    ]

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
    uncurry translate (position tower) $ Pictures
    [ color white $ Line [ (-20, -20), (20, -20), (20, 20), (-20, 20), (-20, -20) ]
    , color white $ Circle 20
    , color white $ Line [ (0, 0), (sin (time * 3.1415 / 60.0) * 20, cos (time * 3.1415 / 60.0) * 20) ]
    ]

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
    | Just pos <- position = uncurry translate (toPair pos) $ color red $ Circle 16
    | otherwise = Blank
    where
        position = pathPoint path $ time * 60.0 + progress

drawPath :: [Segment] -> Picture
drawPath [] = Blank
drawPath (Linear (p, q) : tail) = Pictures [Line [toPair p, toPair q], drawPath tail]
drawPath (Spherical (p, q, c) : tail) = Pictures
    [ uncurry translate (toPair c) $ Arc pa qa (distance p c)
    , drawPath tail
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