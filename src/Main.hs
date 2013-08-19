module Main where

import Control.Monad (unless)
import UI.NCurses

import Game
import Structures

renderTick :: Window -> Grid -> Curses ()
renderTick w g =
  unless (isDead g) $ do
      updateWindow w $ do
          moveCursor 0 0
          drawBox (Just glyphLineV) (Just glyphLineH)
          drawString $ prettyShow g
      render
      renderTick w $ tick g

provisionGrid :: Int -> Grid
provisionGrid s = insertStructureAt (10, 10) (toStructure gosperGlider) $ deadGrid s

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  sc <- screenSize
  let s = fromInteger $ (uncurry min) sc
  renderTick w $ provisionGrid $ s - 1
