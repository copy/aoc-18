import Data.List (foldl')
import qualified Data.Map as Map

data Zippy a =
  Zippy [a]
        [a]

instance Show a => Show (Zippy a) where
  show (Zippy left right) =
    "Zippy(" ++ show (reverse left) ++ " || " ++ show right ++ ")"

insert x (Zippy left right) = Zippy (x : left) right

remove (Zippy (_:left) right) = Zippy left right

current (Zippy (x:_) (_)) = x

from_list xs = Zippy xs []

to_list (Zippy left right) = reverse left ++ right

left (Zippy (x:left) right) = Zippy left (x : right)
left (Zippy [] right) = Zippy (tail $ reverse right) [last right]

right (Zippy left (x:right)) = Zippy (x : left) (right)
right (Zippy [] []) = Zippy [] []
right (Zippy left []) = Zippy [last left] (tail $ reverse left)

left_by n z = iterate left z !! n

right_by n z = iterate right z !! n

main = do
  contents <- readFile "./input9"
  let [players', "players;", "last", "marble", "is", "worth", points', "points"] =
        words contents
  let players = read players' :: Int
  let points = read points' :: Int
  --let (players, points) = (30, 5807)
  let multiplier = 100
  let x =
        foldl'
          (\(score, _, circle) (marble, player) ->
             if marble `mod` 23 == 0
               then let score' =
                          Map.insertWith
                            (+)
                            player
                            (marble + (current $ left_by 7 circle))
                            score
                     in (score', player, right $ remove $ left_by 7 circle)
               else (score, player, insert marble $ right circle))
          (Map.empty, 0, from_list [0])
          (zip [1 .. multiplier * points] $ cycle [1 .. players])
  print (players, points)
  --putStrLn $ unlines $ map show x
  let (score, _, _) = x
  print $ maximum $ map snd $ Map.toList score
