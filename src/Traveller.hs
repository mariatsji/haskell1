module Traveller
(
shortestPath
)
where

import qualified Data.List as L

data Road = Up | Cross | Down deriving (Show, Ord, Eq, Read)

-- [1,2,3 3,2,1 1,2,4]
permutations :: Road -> [Int] -> [[Int]]
permutations _ [] = []
permutations Up (a:b:c:[]) = [[a],[b,c]]
permutations Down (a:b:c:[]) = [[c],[b,a]]
permutations Up (a:b:c:xs) = map (a :) (permutations Up xs) ++ map ([b,c] ++) (permutations Down xs)
permutations Down (a:b:c:xs) = map (c :) (permutations Down xs) ++ map ([b,a] ++) (permutations Up xs)

permutations' :: [Int] -> [[Int]]
permutations' xs = permutations Up xs ++ permutations Down xs

shortestPath' :: [Int] -> Int
shortestPath' xs = minimum $ map sum $ permutations' xs

shortestPath :: [Int] -> [Int]
shortestPath xs = head $ L.sortBy comp (permutations' xs)

comp :: [Int] -> [Int] -> Ordering
comp a b = compare (sum a) (sum b)