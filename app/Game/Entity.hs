module Game.Entity
    ( initial
    , addTower
    , defaultTower
    , addEnemy
    , defaultEnemy
    , setPath
    ) where


import Control.Lens
import qualified Data.Map.Strict as M

import Linear.V2

import Game.Util

import Game.Components.GameState
import Game.Components.Enemy as Enemy
import Game.Components.Path as Path
import Game.Components.Tower as Tower

initial :: GameState
initial = GameState
    { _towers = M.empty
    , _enemies = M.empty
    , _path = Path []
    , _time = 0
    , _nextId = 1
    }

defaultTower :: Tower
defaultTower = Tower
    { Tower._entityId = TowerId 0
    , Tower._position = V2 0 0
    , _range = 90
    , _area = []
    , _targets = []
    }
addTower :: Tower -> GameState -> GameState
addTower tower gs =
    let id' = TowerId (gs ^. nextId)
        area' = calcArea (gs ^. path) (toPair (tower ^. Tower.position)) (tower ^. range)
        tower' = tower & area .~ area' & Tower.entityId .~ id'
    in  gs
        & towers %~ M.insert id' tower'
        & nextId +~ 1

defaultEnemy :: Enemy
defaultEnemy = Enemy
    { Enemy._entityId = EnemyId 0
    , _progress = 0
    , Enemy._position = Nothing
    , _health = 30
    }

addEnemy :: Enemy -> GameState -> GameState
addEnemy enemy gs =
      let id' = EnemyId (gs ^. nextId)
          enemy' = enemy & Enemy.entityId .~ id'
      in  gs
          & enemies %~ M.insert id' enemy'
          & nextId +~ 1

setPath :: Path.Path -> GameState -> GameState
setPath path' gs = gs & path .~ path'