module Main where

import Control.Monad (unless)
import UI.NCurses

import Game

renderTick :: Window -> Grid -> Curses ()
renderTick w g =
  unless (isDead g) $ do
      updateWindow w $ do
          moveCursor 0 0
          drawString $ show g
      render
      renderTick w (tick g)

main :: IO ()
main = runCurses $ do 
  setEcho False
  w <- defaultWindow
  renderTick w provisionGrid