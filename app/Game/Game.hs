{-# LANGUAGE TemplateHaskell #-}
module Game.Game
    ( GameState(..)
    , initialize
    , input
    , update
    , render
    ) where

import Control.Lens

import qualified Data.Map.Strict as M

import Debug.Trace

import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.IO.Game
import Linear.V2

import Game.Type
import Game.Graphics

import Game.Entity
import Game.Components.Enemy as Enemy
import Game.Components.Tower as Tower
import Game.Components.Path as Path
import Game.Components.GameState

import Game.Systems.EnemyUpdate
import Game.Systems.TowerAttack
import Game.Systems.TowerTargeting

pathPoints :: [Float2]
pathPoints = map (uncurry V2) [ (-360, -60), (-150, -60), (-90, 60), (90, 60), (150, -60), (360, -60), (360, 60), (150, 60), (90, -60), (-90, -60), (-150, 60), (-360, 60) ]

initialize :: GameState
initialize =
      setPath (createPath pathPoints)
    $ addTower (defaultTower { Tower._position = V2 (-180) (0), _range = 80 })
    $ addTower (defaultTower { Tower._position = V2 (   0) (0), _range = 80 })
    $ addTower (defaultTower { Tower._position = V2 ( 180) (0), _range = 80 })
    $ addEnemy (defaultEnemy { _progress =   -0 })
    $ addEnemy (defaultEnemy { _progress =  -60 })
    $ addEnemy (defaultEnemy { _progress = -120 })
    $ addEnemy (defaultEnemy { _progress = -180 })
    $ addEnemy (defaultEnemy { _progress = -240 })
      initial

input :: Event -> GameState -> GameState
input _e gs = gs

update :: Float -> GameState -> GameState
update delta gs =
      tick delta
    $ updateEnemies delta
    $ updateTowerTargets
    $ updateAttack delta
    $ removeDeadEnemies
      gs

tick :: Float -> GameState -> GameState
tick delta gs = gs & time +~ delta

render :: GameState -> Gloss.Picture
render gs = Gloss.Pictures $
       [Gloss.Color Gloss.yellow $ drawPath (gs ^. (path . segments))]
    ++ map (drawTower (gs ^. time)) (M.elems $ gs ^. towers)
    ++ map (drawEnemy (gs ^. time) (gs ^. (path . segments))) (M.elems $ gs ^. enemies)