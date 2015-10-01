module FlipFlop
(flipflop)
where

import Data.Char

flipflop :: String -> [Bool] -> String
flipflop s b = listMap (map flipflop' s) b
    where
        flipflop' x y = if y then toUpper x else x

listMap :: [(a -> b)] -> [a] -> [b]
listMap [] _ = []
listMap _ [] = []
listMap (f:fs) (x:xs) = f x : listMap fs xs

-- main = mapM_ print $ flipflop "abcdefg" $ cycle [False, True]


