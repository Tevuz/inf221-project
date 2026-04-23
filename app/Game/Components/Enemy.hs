module Game.Components.Enemy (Enemy(..), defaultEnemy, alive) where

import Game.Util

data Enemy = Enemy { progress :: Float, position :: Maybe Float2, health :: Float }

defaultEnemy :: Enemy
defaultEnemy = Enemy
    { progress = 0
    , position = Nothing
    , health = 20
    }

alive :: Enemy -> Bool
alive enemy = 0 < health enemy

