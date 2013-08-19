module Structures where

import Game (Structure)

toStructure :: [String] -> Structure
toStructure = concatMap (uncurry toRepr) . zip [0..]
  where toRepr y = map (\(x, _) -> (x, y)) . filter ((== '*') . snd) . zip [0..]

blinker :: Structure
blinker = zip (repeat 0) [0..3]

glider = [".*."
         ,"..*"
         ,"***"
         ]

gosperGlider = ["........................*"
               ,"......................*.*"
               ,"............**......**............**"
               ,"...........*...*....**............**"
               ,"**........*.....*...**"
               ,"**........*...*.**....*.*"
               ,"..........*.....*.......*"
               ,"...........*...*"
               ,"............**"
               ]
