{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec

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

square :: Float -> Float
square x = x * x

distance :: Point -> Point -> Float
distance (x0, y0) (x1, y1) = sqrt (square (x1 - x0) + square (y1 - y0))

pathRadius :: Float
pathRadius = 20

pathPoints :: Path
pathPoints = [ (-360, -60), (-120, -60), (-60, 60), (60, 60), (120, -60), (360, -60) ]
--pathPoints = [ (-300, -60), (-60, -60), (0, 60), (60, -60), (300, -60) ]

data Segment
    = Linear (Point, Point)
    | Spherical (Point, Point, Point)

pathInsertCorners :: Path -> [Segment]
pathInsertCorners [] = []
pathInsertCorners [a] = [Linear (a, a)]
pathInsertCorners [a, b] = [Linear (lerp 0.5 a b, b)]
pathInsertCorners (a : b : c : tail)
    | abs det < 1e-8 = Linear (am, cm) : pathInsertCorners (b : c : tail)
    | otherwise      = [Linear (am, b0), Spherical (b0, b1, b'), Linear (b1, cm)] ++ pathInsertCorners (b : c : tail)
    where
        r = pathRadius
        lab = distance a b
        lcd = distance b c
        am = lerp 0.5 a b
        b0 = lerp (1 - r / lab) a b
        b1 = lerp (    r / lcd) b c
        cm = lerp 0.5 b c
        baT = (\ (x, y) -> (-y, x)) (a Vec.- b)
        bcT = (\ (x, y) -> (-y, x)) (c Vec.- b)
        det = detV baT bcT
        b' = b0 Vec.+ mulSV (detV (b1 Vec.- b0) bcT / det) baT

pathInsertCornersInit :: Path -> [Segment]
pathInsertCornersInit [] = []
pathInsertCornersInit [a] = [Linear (a, a)]
pathInsertCornersInit [a, b] = [Linear (a, b)]
pathInsertCornersInit (a : b : tail) = Linear (a, lerp 0.5 a b) : pathInsertCorners (a : b: tail)

path :: [Segment]
path = pathInsertCornersInit pathPoints

segmentLength :: Segment -> Float
segmentLength (Linear (p, q)) = distance p q
segmentLength (Spherical (p, q, c)) =
    let p' = p Vec.- c
        q' = q Vec.- c
    in  acos (dotV p' q' / (magV p' * magV q')) * magV p'

pathLength :: Float
pathLength = sum $ map segmentLength path

data Tower = Tower { position :: Point }
towers :: [Tower]
towers =
    [ Tower (0,  0)
    ]

drawTower :: Time -> Tower -> Picture
drawTower time tower =
    uncurry translate (position tower) $ Pictures
    [ color white $ Line [ (-20, -20), (20, -20), (20, 20), (-20, 20), (-20, -20) ]
    , color white $ Circle 20
    , color white $ Line [ (0, 0), (sin (time * 3.1415 / 60.0) * 20, cos (time * 3.1415 / 60.0) * 20) ]
    ]

segmentPoint :: Segment -> Float -> Point
segmentPoint (Linear (p, q)) t = lerp t p q
segmentPoint (Spherical (p, q, c)) t = slerp t (p Vec.- c) (q Vec.- c) Vec.+ c

pathPoint :: [Segment] -> Float -> Maybe Point
pathPoint [] _ = Nothing
pathPoint [segment] t =
    Just $ segmentPoint segment (clamp 0 1 $ t / l)
    where l = segmentLength segment
pathPoint (segment : tail) t
    | t <= l = Just $ segmentPoint segment (t / l)
    | otherwise = pathPoint tail (t - l)
    where l = segmentLength segment

drawPath :: [Segment] -> Picture
drawPath [] = Blank
drawPath (Linear (p, q) : tail) = Pictures [Line [p, q], drawPath tail]
drawPath (Spherical (p, q, c) : tail) = Pictures
    [ uncurry translate c $ uncurry Arc (minmax (pa, qa)) r
    , drawPath tail
    ]
    where
        r = magV (p Vec.- c)
        p' = p Vec.- c
        q' = q Vec.- c
        pa = angle p'
        qa = angle q'

lerp :: Float -> Point -> Point -> Point
lerp i (x0, y0) (x1, y1) = (x0 + i * (x1 - x0), y0 + i * (y1 - y0))

slerp :: Float -> Point -> Point -> Point
slerp i p q =
    let theta = acos (dotV p q / (magV p * magV q))
        pf    = sin (theta - i * theta) / sin theta
        qf    = sin (        i * theta) / sin theta
    in  mulSV pf p Vec.+ mulSV qf q

clamp :: Float -> Float -> Float -> Float
clamp a b x = max a $ min b x

angle :: Point -> Float
angle (x, y) = radToDeg (atan2 y x)

minmax :: (Float, Float) -> (Float, Float)
minmax (a, b)
    | a < b     = (a, b)
    | otherwise = (b, a)