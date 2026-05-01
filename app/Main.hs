{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Graphics.Gloss

import Game.Game

main :: IO ()
main = play window background 60 initialize render input update

window :: Display
window = InWindow "Tower Defence" (800, 600) (100, 100)

background :: Color
background = black