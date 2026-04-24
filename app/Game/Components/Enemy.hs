{-# LANGUAGE TemplateHaskell #-}
module Game.Components.Enemy
    (Enemy(..)
    , defaultEnemy
    , alive
    , progress
    , position
    , health
    ) where

import Control.Lens

import Game.Util

data Enemy = Enemy
    { _progress :: Float
    , _position :: Maybe Float2
    , _health :: Float
    }
makeLenses ''Enemy

defaultEnemy :: Enemy
defaultEnemy = Enemy
    { _progress = 0
    , _position = Nothing
    , _health = 20
    }

alive :: Enemy -> Bool
alive enemy = enemy ^. health < 0

