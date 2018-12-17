import Data.List (maximumBy, minimumBy)
import Data.List ((\\), nub, sortBy)
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

manhatten (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

closest point points =
  let (closest:second_closest:_) = sortBy (comparing (manhatten point)) points
   in if manhatten closest point == manhatten second_closest point
        then Nothing
        else Just closest

main
  --contents <- readFile "./input6"
 = do
  contents <- readFile "./input6"
  let pair [x, y] = (read x :: Int, read y :: Int)
  let coords =
        map (pair . filter (/= "") . Split.splitOneOf ", ") $ lines contents
  let (min_x, max_x, min_y, max_y) =
        ( minimum $ map fst coords
        , maximum $ map fst coords
        , minimum $ map snd coords
        , maximum $ map snd coords)
  let mappy =
        foldl
          (\map (x, y) -> Map.insert (x, y) (closest (x, y) coords) map)
          Map.empty
          [(x, y) | x <- [min_x .. max_x], y <- [min_y .. max_y]]
  let coords_at_boundary =
        nub $
        catMaybes $
        map snd $
        filter
          (\((x, y), _) -> x == min_x || x == max_x || y == min_y || y == max_y) $
        Map.toList mappy
  let not_infinite = coords \\ coords_at_boundary
  print coords
  print (min_x, max_x, min_y, max_y)
  --print mappy
  print coords_at_boundary
  print not_infinite
  print $
    maximumBy (comparing snd) $
    zip not_infinite $
    map
      (\p -> length $ filter (== Just p) $ map snd $ Map.toList mappy)
      not_infinite
  print $
    length $
    filter
      (\p -> sum (map (manhatten p) coords) < 10000)
      [(x, y) | x <- [min_x .. max_x], y <- [min_y .. max_y]]
