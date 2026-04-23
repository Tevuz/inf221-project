module Game.Game
    ( GameState(..)
    , input
    , update
    , render
    , initial
    ) where

import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.IO.Game
import Linear.V2

import Game.Type
import Game.Domain.Enemy
import Game.Domain.Path
import Game.Domain.Tower
import Game.Graphics
import qualified Game.Domain.Path as Path

data GameState = GameState
    {   towers :: [Tower]
    ,   enemies :: [Enemy]
    ,   path :: Game.Domain.Path.Path
    ,   time :: Float
    }


input :: Event -> GameState -> GameState
input _e gs = gs

update :: Float -> GameState -> GameState
update delta gs = gs
    { enemies = map (updateEnemy delta) (enemies gs)
    , time = time gs + delta
    }

render :: GameState -> Gloss.Picture
render gs = Gloss.Pictures $
       [Gloss.Color Gloss.yellow $ drawPath (segments (path gs))]
    ++ map (drawTower (time gs)) (towers gs)
    ++ map (drawEnemy (time gs) (segments (path gs))) (enemies gs)


initial :: GameState
initial = GameState
    { towers =
        [ createTower (-180,  0) 80 initialPath
        , createTower (   0,  0) 80 initialPath
        , createTower ( 180,  0) 80 initialPath
        ]
    , enemies =
        [ Enemy { progress = -0.0 }
        , Enemy { progress = -60.0 }
        , Enemy { progress = -120.0 }
        , Enemy { progress = -180.0 }
        , Enemy { progress = -240.0 }
        ]
    , path = initialPath
    , time = 0
    }

pathPoints :: [Float2]
pathPoints = map (uncurry V2) [ (-360, -60), (-150, -60), (-90, 60), (90, 60), (150, -60), (360, -60), (360, 60), (150, 60), (90, -60), (-90, -60), (-150, 60), (-360, 60) ]
initialPath :: Path.Path
initialPath = createPath pathPoints