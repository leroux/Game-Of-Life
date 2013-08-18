module Main where

import Control.Monad (unless)
import UI.NCurses

import Game
import Structures

renderTick :: Window -> Grid -> Curses ()
renderTick w g =
  unless (isDead g) $ do
      c <- newColorID ColorRed ColorWhite =<< maxColorID
      updateWindow w $ do
          moveCursor 0 0
          setColor c
          drawString $ show g
      render
      renderTick w (tick g)

provisionGrid :: Grid
provisionGrid = insertStructureAt (10, 10) (toStructure gosperGlider) (deadGrid 50 50)

main :: IO ()
main = runCurses $ do 
  setEcho False
  w <- defaultWindow
  renderTick w $ provisionGrid
