import Data.List (elemIndex, minimumBy, scanl')
import Data.List.Split (splitOneOf)
import Data.Ord (comparing)

bounds points =
  ( minimum $ map fst points
  , minimum $ map snd points
  , maximum $ map fst points
  , maximum $ map snd points)

absolute (min_x, min_y, max_x, max_y) = (max_x - min_x) * (max_y - min_y)

show_map points =
  let (min_x, min_y, max_x, max_y) = bounds points
   in unlines $
      map
        (\y ->
           map
             (\x ->
                if (x, y) `elem` points
                  then '#'
                  else '.')
             [min_x .. max_x])
        [min_y .. max_y]

main = do
  contents <- readFile "./input10"
  let r x = read x :: Int
  let parse ["position", px, py, "velocity", vx, vy] = (r px, r py, r vx, r vy)
  let initial =
        map (parse . filter (/= "") . splitOneOf "=,<> ") $ lines contents
  let step (px, py, vx, vy) = (px + vx, py + vy, vx, vy)
  let steps =
        map (map (\(x, y, _, _) -> (x, y))) $
        scanl' (\mappy _ -> map step mappy) initial [1 .. 100000]
  let smallest = minimumBy (comparing (absolute . bounds)) steps
  putStrLn $ show_map $ smallest
  print $ elemIndex smallest steps
