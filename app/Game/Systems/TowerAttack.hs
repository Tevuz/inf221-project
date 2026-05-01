module Game.Systems.TowerAttack (updateAttack) where

import Control.Lens
import qualified Data.Map.Strict as M

import Game.Components.Tower
import Game.Components.Enemy
import Game.Components.GameState

updateAttack :: Float -> GameState -> GameState
updateAttack delta gs =
    foldl (flip (damageEnemy delta)) gs (concatMap (^. targets) (M.elems (gs ^. towers)))

damageEnemy :: Float -> EnemyId -> GameState -> GameState
damageEnemy delta enemyId = enemies %~ M.adjust (applyDamage delta) enemyId

applyDamage :: Float -> Enemy -> Enemy
applyDamage delta enemy = enemy & health -~ delta * 8.0