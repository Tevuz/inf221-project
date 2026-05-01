{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Game.Systems.TowerTargeting (updateTowerTargets) where

import Control.Lens
import qualified Data.Map.Strict as M

import Linear.Metric

import Game.Components.Enemy as Enemy
import Game.Components.Tower as Tower
import Game.Components.GameState

data Strategy = First | Last -- | Closest | Weakest | Strongest

updateTowerTargets :: GameState -> GameState
updateTowerTargets gs =
    gs & towers %~ M.map (updateTowerTarget First gs)

updateTowerTarget :: Strategy -> GameState -> Tower -> Tower
updateTowerTarget strategy gs tower =
    tower & targets .~ select strategy tower (candidates gs tower)

select :: Strategy -> Tower -> [Enemy] -> [EnemyId]
select _ _ [] = []
select First _ (e:_) = [e ^. Enemy.entityId]
select Last  _ es    = [last es ^. Enemy.entityId]

candidates :: GameState -> Tower -> [Enemy]
candidates gs tower = filter (\e -> alive e && inRange tower e)  (M.elems (gs ^. enemies))

inRange :: Tower -> Enemy -> Bool
inRange tower enemy =
  case enemy ^. Enemy.position of
    Just ep -> distance ep (tower ^. Tower.position) <= tower ^. range
    Nothing -> False



