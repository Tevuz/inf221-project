{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Game.Graphics (drawEnemy, drawPath, drawTower) where

import Control.Lens

import Graphics.Gloss
import Linear.Metric

import Game.Util
import Game.Type

import Game.Components.Enemy as Enemy
import Game.Components.Tower as Tower
import Game.Components.Path as Path

drawEnemy :: Time -> [Segment] -> Enemy -> Picture
drawEnemy _ path enemy
    | Just pos <- position = uncurry translate (toPair pos) $ color col $ Circle 16
    | otherwise = Blank
    where
        position = pathPoint path $ enemy ^. progress
        col = makeColor 1.0 (enemy ^. health / 20.0) 0.0 1.0

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
    [   uncurry translate (toPair $ tower ^. Tower.position) $ Pictures
        [   color white $ Line [ (-20, -20), (20, -20), (20, 20), (-20, 20), (-20, -20) ]
        ,   color white $ Circle 20
        ,   color cyan $ Circle (tower ^. range)
        ]
--    ,   let ep = tower ^? target . _Just . Enemy.position
--            tp = toPair $ tower ^. Tower.position
--        in case ep of
--            Just (Just ep) -> color cyan $ Line [tp, toPair ep]
--            _ -> Blank
    ]

--inRange :: Time -> Enemy -> Tower -> Bool
--inRange time enemy tower = any (\(a, b) -> a < t && t < b) (area Tower)
--    where
--        t = time * 60 + progress enemy