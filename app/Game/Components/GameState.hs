{-# LANGUAGE TemplateHaskell #-}
module Game.Components.GameState
    ( GameState(..)
    , towers
    , enemies
    , path
    , time
    , nextId
    ) where

import Control.Lens
import qualified Data.Map.Strict as M

import Game.Components.Enemy
import Game.Components.Path
import Game.Components.Tower

data GameState = GameState
    { _towers :: M.Map TowerId Tower
    , _enemies :: M.Map EnemyId Enemy
    , _path :: Game.Components.Path.Path
    , _time :: Float
    , _nextId :: Int
    }
makeLenses ''GameState