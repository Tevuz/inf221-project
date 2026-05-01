{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Game.Systems.EnemyUpdate (updateEnemies, removeDeadEnemies) where

import Control.Lens
import qualified Data.Map.Strict as M

import Game.Components.Enemy
import Game.Components.Path
import Game.Components.GameState

speed :: Float
speed = 60

updateEnemies :: Float -> GameState -> GameState
updateEnemies delta gs =
    gs & enemies %~ M.map (updateEnemyProgress delta . updateEnemyPosition (gs ^. path))

updateEnemyProgress :: Float -> Enemy -> Enemy
updateEnemyProgress delta enemy = enemy & progress +~ (delta * speed)

updateEnemyPosition :: Path -> Enemy -> Enemy
updateEnemyPosition path enemy = enemy & position .~ pathPoint (path ^. segments) (enemy ^. progress)

removeDeadEnemies :: GameState -> GameState
removeDeadEnemies gs =
    gs & enemies %~ M.mapMaybe (\e -> if alive e then Just e else Nothing)