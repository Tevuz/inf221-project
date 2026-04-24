{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Game.Systems.EnemyUpdate (updateEnemies) where

import Control.Lens

import Game.Components.Enemy
import Game.Components.Path
import Game.Components.GameState

speed :: Float
speed = 60

updateEnemies :: Float -> GameState -> GameState
updateEnemies delta gs =
    gs & enemies %~ map (updateEnemyProgress delta . updateEnemyPosition (gs ^. path))

updateEnemyProgress :: Float -> Enemy -> Enemy
updateEnemyProgress delta enemy = enemy & progress +~ (delta * speed)

updateEnemyPosition :: Path -> Enemy -> Enemy
updateEnemyPosition path enemy = enemy & position .~ pathPoint (path ^. segments) (enemy ^. progress)