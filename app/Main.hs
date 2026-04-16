{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Graphics.Gloss

type Time = Float

main :: IO ()
main = animate window background frame

window :: Display
window = InWindow "Tower Defence" (800, 600) (100, 100)

background :: Color
background = black

frame :: Time -> Picture
frame time = Pictures $
    map (drawTower time) towers


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