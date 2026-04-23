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
import Game.Graphics

import Game.Components.Enemy as Enemy
import Game.Components.Path as Path
import Game.Components.Tower as Tower
import Game.Components.GameState

import Game.Systems.EnemyUpdate
import Game.Systems.TowerTargeting


input :: Event -> GameState -> GameState
input _e gs = gs

update :: Float -> GameState -> GameState
update delta gs =
      tick delta
    $ updateEnemies delta
    $ updateTowerTargets
    gs

tick :: Float -> GameState -> GameState
tick delta gs = gs { time = time gs + delta }

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
        [ defaultEnemy { progress =   -0.0, Enemy.position = Nothing }
        , defaultEnemy { progress =  -60.0, Enemy.position = Nothing  }
        , defaultEnemy { progress = -120.0, Enemy.position = Nothing  }
        , defaultEnemy { progress = -180.0, Enemy.position = Nothing  }
        , defaultEnemy { progress = -240.0, Enemy.position = Nothing  }
        ]
    , path = initialPath
    , time = 0
    }

pathPoints :: [Float2]
pathPoints = map (uncurry V2) [ (-360, -60), (-150, -60), (-90, 60), (90, 60), (150, -60), (360, -60), (360, 60), (150, 60), (90, -60), (-90, -60), (-150, 60), (-360, 60) ]
initialPath :: Path.Path
initialPath = createPath pathPoints