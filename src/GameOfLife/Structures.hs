module Structures where

import Game (Structure)

toStructure :: [String] -> Structure
toStructure = concatMap (uncurry toRepr) . zip [0..]
  where toRepr y = map (\(x, _) -> (x, y)) . filter ((== '*') . snd) . zip [0..]

blinker :: Structure
blinker = zip (repeat 0) [0..3]

glider = [".*.",
          "..*",
          "***"]

brilliant = [(1, 0),
             (3, 1),
             (0, 2), (1, 2), (4, 2), (5, 2), (6, 2)]

line len = zip [0..len] (repeat 0)

infinite = [(0, 0), (1, 0), (2, 0), (4, 0),
            (0, 1),
            (3, 2), (4, 2),
            (1, 3), (2, 3), (4, 3),
            (0, 4), (2, 4), (4, 4)]

gosperGlider = ["........................*"
               ,"......................*.*"
               ,"............**......**............**"
               ,"...........*...*....**............**"
               ,"**........*.....*...**"
               ,"**........*...*.**....*.*"
               ,"..........*.....*.......*"
               ,"...........*...*"
               ,"............**"]
