module Game.Domain.Enemy (Enemy(..), updateEnemy) where

import Game.Type

data Enemy = Enemy { progress :: Float }

speed = 60

updateEnemy :: Float -> Enemy -> Enemy
updateEnemy delta enemy = enemy
    { progress = progress enemy + delta * speed
    }
