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
    map (tower time) towers

towers :: [Point]
towers =
    [ ( 0,  0)
    , (90, 30)
    , (30, 90)
    ]

tower :: Time -> Point -> Picture
tower time position =
    uncurry translate position $ Pictures
    [ color white $ Circle 20
    , color white $ Line [ (0, 0), (sin (time * 3.1415 / 60.0) * 20, cos (time * 3.1415 / 60.0) * 20) ]
    ]