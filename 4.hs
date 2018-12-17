import Control.Exception.Base
import Data.Char
import Data.Foldable
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import Data.Ord
import Data.Sort

data Entry
  = Falls
  | Wakes
  | Guard Int
  deriving (Show, Eq)

make_pair [a, b] = (a, b)

parse_entry " falls asleep" = Falls
parse_entry " wakes up" = Wakes
parse_entry guard = Guard $ (read (filter isDigit guard) :: Int)

parse_date date =
  case filter (/= "") $ Split.splitOneOf "[- :" date of
    [y, m, d, hour, minute] -> (r y, r m, r d, r hour, r minute)
  where
    r i = read i :: Int

actions_to_map [] = []
actions_to_map (((_, _, _, start_h, start_m), Falls):((_, _, _, end_h, end_m), Wakes):rest) =
  minutes (start_h, start_m) (end_h, end_m) ++ actions_to_map rest

minutes (from_hours, from_minutes) (to_hours, to_minutes) =
  if (from_hours, from_minutes) == (to_hours, to_minutes)
    then []
    else assert ((from_hours, from_minutes) < (to_hours, to_minutes)) $
         from_minutes : minutes new_from (to_hours, to_minutes)
  where
    new_from =
      if from_minutes == 59
        then (from_hours + 1, 0)
        else (from_hours, from_minutes + 1)

get_max_minute map =
  if Map.null map
    then (0, 0)
    else maximumBy (comparing snd) $ Map.toList map

main = do
  contents <- readFile "./input4"
  let entries =
        Map.toList $
        foldl
          (\mappy (guard, actions) ->
             Map.unionWith (Map.unionWith (+)) mappy $
             Map.singleton guard $
             Map.fromListWith (+) $ zip (actions_to_map actions) (repeat 1))
          Map.empty $
        map (\((date, Guard n):actions) -> (n, actions)) $
        filter (/= []) $
        Split.split
          (Split.keepDelimsL $
           Split.whenElt (\(_, x) -> x /= Falls && x /= Wakes)) $
        sortOn fst $
        map
          ((\(date, msg) -> (parse_date date, parse_entry msg)) .
           make_pair . Split.splitOneOf "]") $
        lines contents
  let (max_id, max_map) =
        maximumBy (comparing $ sum . map snd . Map.toList . snd) entries
  let (max_minute, count) = maximumBy (comparing snd) $ Map.toList max_map
  print (max_id, max_minute)
  print (max_id * max_minute)
  let (max_id2, max_map2) =
        maximumBy (comparing (snd . get_max_minute . snd)) entries
  let max_minute2 = fst $ get_max_minute max_map2
  print $ map (get_max_minute . snd) entries
  print max_map2
  print (max_id2, max_minute2)
  print (max_id2 * max_minute2)
