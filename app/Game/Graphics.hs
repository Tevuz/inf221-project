{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Game.Graphics (drawEnemy, drawPath, drawTower) where

import Graphics.Gloss
import Linear.Vector
import Linear.Metric

import Game.Util
import Game.Type

import Game.Domain.Enemy
import Game.Domain.Path
import Game.Domain.Tower

drawEnemy :: Time -> [Segment] -> Enemy -> Picture
drawEnemy time path enemy
    | Just pos <- position = uncurry translate (toPair pos) $ color red $ Circle 16
    | otherwise = Blank
    where
        position = pathPoint path $ progress enemy
--        col = if any (inRange time enemy) towers then green else red

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

drawTower :: Time -> Tower -> Picture
drawTower time tower =
    Pictures
    [   uncurry translate (position tower) $ Pictures
        [   color white $ Line [ (-20, -20), (20, -20), (20, 20), (-20, 20), (-20, -20) ]
        ,   color white $ Circle 20
        ,   color cyan $ Circle (range tower)
        ,   color white $ Line [ (0, 0), (sin (time * 3.1415 / 60.0) * 20, cos (time * 3.1415 / 60.0) * 20) ]
        ]
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

--inRange :: Time -> Enemy -> Tower -> Bool
--inRange time enemy tower = any (\(a, b) -> a < t && t < b) (area Tower)
--    where
--        t = time * 60 + progress enemy