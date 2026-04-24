{-# LANGUAGE TemplateHaskell #-}
module Game.Components.GameState
    ( GameState(..)
    , towers
    , enemies
    , path
    , time
    ) where

import Control.Lens

import Game.Components.Enemy
import Game.Components.Path
import Game.Components.Tower

data GameState = GameState
    { _towers :: [Tower]
    , _enemies :: [Enemy]
    , _path :: Game.Components.Path.Path
    , _time :: Float
    }
makeLenses ''GameState