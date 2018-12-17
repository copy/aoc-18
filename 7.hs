import Data.Char (ord)
import Data.List ((\\), nub, sort, unfoldr)
import Data.List.Split (splitOneOf)
import Data.Maybe (mapMaybe)

parse l =
  case splitOneOf " " l of
    ["Step", [before], "must", "be", "finished", "before", "step", [after], "can", "begin."] ->
      (before, after)

data Status
  = Not_started
  | Steps_left Int
  deriving (Show, Eq)

decrement_steps (Steps_left 0) = error "steps can't be negative"
decrement_steps (Steps_left n) = (Steps_left (n - 1))
decrement_steps x = x

main = do
  contents <- readFile "./input7"
  --contents <- readFile "./input7-example"
  let ins = map parse $ lines contents
  print ins
  let initial = sort $ nub $ map snd ins ++ map fst ins
  print $
    unfoldr
      (\(ins, remaining) ->
         let can_begin = remaining \\ map snd ins
          in case can_begin of
               [] -> Nothing
               (c:_) -> Just (c, (filter ((/= c) . fst) ins, remaining \\ [c])))
      (ins, initial)
  let nr_of_workers = 5
  let steps letter = ord letter - ord 'A' + 61
  print $
    length $
    unfoldr
      (\(ins, remaining, free_workers) ->
         let finished = filter ((== Steps_left 1) . snd) remaining
             free_workers' = free_workers + length finished
             ins' = filter ((`notElem` (map fst finished)) . fst) ins
             (free_workers'', remaining'') =
               foldl
                 (\(workers, acc) (letter, status) ->
                    if status == Not_started &&
                       workers > 0 && letter `notElem` (map snd ins')
                      then ( workers - 1
                           , (letter, Steps_left $ steps letter) : acc)
                      else (workers, (letter, decrement_steps status) : acc))
                 (free_workers', [])
                 (remaining \\ finished)
          in if remaining'' == []
               then Nothing
               else Just (remaining'', (ins', remaining'', free_workers'))) $
    (ins, zip initial (repeat Not_started), nr_of_workers)
