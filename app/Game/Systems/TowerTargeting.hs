{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Game.Systems.TowerTargeting (updateTowerTargets) where

import Control.Lens

import Linear.Metric

import Game.Components.Enemy as Enemy
import Game.Components.Tower as Tower
import Game.Components.GameState

data Strategy = First | Last -- | Closest | Weakest | Strongest

updateTowerTargets :: GameState -> GameState
updateTowerTargets gs =
    gs & towers %~ map (updateTowerTarget First (gs ^. enemies))

updateTowerTarget :: Strategy -> [Enemy] -> Tower -> Tower
updateTowerTarget strategy enemies tower =
    tower & target %~ const (select strategy tower (candidates tower enemies))

select :: Strategy -> Tower -> [Enemy] -> Maybe Enemy
select _ _ [] = Nothing
select First _ enemies = Just (head enemies)
select Last _ enemies = Just (last enemies)

candidates :: Tower -> [Enemy] -> [Enemy]
candidates tower = filter (\e -> all ($ e) [inRange tower, alive])

inRange :: Tower -> Enemy -> Bool
inRange tower enemy
    | Just ep <- ep = distance ep tp <= tower ^. range
    | otherwise     = False
    where
        ep = enemy ^. Enemy.position
        tp = tower ^. Tower.position



