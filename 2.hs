{-# LANGUAGE TupleSections #-}

import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Map as Map

counts word = foldl (\map chr -> Map.insertWith (+) chr 1 map) Map.empty word

pairs [] = []
pairs [x] = []
pairs (x:xs) = map (x, ) xs ++ pairs xs

difference [] [] = 0
difference (x:xs) (y:ys) =
  (if x == y
     then 0
     else 1) +
  difference xs ys

same_characters [] [] = []
same_characters (x:xs) (y:ys) =
  (if x == y
     then [x]
     else []) ++
  same_characters xs ys

main :: IO ()
main = do
  contents <- readFile "input2"
  let values = map (map snd . Map.toList . counts) $ lines contents
  let twos = length $ filter (\counts -> 2 `elem` counts) values
  let threes = length $ filter (\counts -> 3 `elem` counts) values
    --print values
  print (twos * threes)
  putStrLn "------------"
  print $ map (uncurry difference) $ pairs $ lines contents
  let min = minimumBy (compare `on` uncurry difference) $ pairs $ lines contents
  print min
  print $ uncurry same_characters min
