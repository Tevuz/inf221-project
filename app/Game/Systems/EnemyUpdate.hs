{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Game.Systems.EnemyUpdate (updateEnemies) where

import Game.Components.Enemy
import Game.Components.Path
import Game.Components.GameState

speed :: Float
speed = 60

updateEnemies :: Float -> GameState -> GameState
updateEnemies delta gs = gs
    { enemies = map (updateEnemyProgress delta . updateEnemyPosition (path gs)) (enemies gs) }

updateEnemyProgress :: Float -> Enemy -> Enemy
updateEnemyProgress delta enemy = enemy
    { progress = progress enemy + delta * speed }

updateEnemyPosition :: Path -> Enemy -> Enemy
updateEnemyPosition path enemy = enemy
    { position = pathPoint (segments path) (progress enemy) }