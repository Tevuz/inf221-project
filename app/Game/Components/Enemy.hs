{-# LANGUAGE TemplateHaskell #-}
module Game.Components.Enemy
    (Enemy(..)
    , alive
    , progress
    , position
    , health
    , EnemyId(..)
    , entityId
    ) where

import Control.Lens

import Game.Util

newtype EnemyId = EnemyId Int
    deriving (Show, Eq, Ord)

data Enemy = Enemy
    { _entityId :: EnemyId
    , _progress :: Float
    , _position :: Maybe Float2
    , _health :: Float
    } deriving (Show)
makeLenses ''Enemy

alive :: Enemy -> Bool
alive enemy = 0 < enemy ^. health

