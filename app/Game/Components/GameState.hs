module Game.Components.GameState (GameState(..)) where

import Game.Components.Enemy
import Game.Components.Path
import Game.Components.Tower

data GameState = GameState
    {   towers :: [Tower]
    ,   enemies :: [Enemy]
    ,   path :: Game.Components.Path.Path
    ,   time :: Float
    }