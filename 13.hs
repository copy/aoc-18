import Data.List (sort)
import Data.List.Unique (repeated)
import Data.Maybe (isJust, listToMaybe, mapMaybe)

field ' ' = ' '
field '/' = '/'
field '\\' = '\\'
field '+' = '+'
field '|' = '|'
field '-' = '-'
field 'v' = '|'
field '^' = '|'
field '<' = '-'
field '>' = '-'

piece 'v' = Just 'v'
piece '^' = Just '^'
piece '<' = Just '<'
piece '>' = Just '>'
piece _ = Nothing

delta 'v' = (0, 1)
delta '^' = (0, -1)
delta '<' = (-1, 0)
delta '>' = (1, 0)

get_new_direction' 'v' '|' = 'v'
get_new_direction' 'v' '\\' = '>'
get_new_direction' 'v' '/' = '<'
get_new_direction' '^' '|' = '^'
get_new_direction' '^' '\\' = '<'
get_new_direction' '^' '/' = '>'
get_new_direction' '<' '-' = '<'
get_new_direction' '<' '\\' = '^'
get_new_direction' '<' '/' = 'v'
get_new_direction' '>' '-' = '>'
get_new_direction' '>' '\\' = 'v'
get_new_direction' '>' '/' = '^'

get_new_direction 0 'v' '+' = (1, '>')
get_new_direction 1 'v' '+' = (2, 'v')
get_new_direction 2 'v' '+' = (0, '<')
get_new_direction 0 '^' '+' = (1, '<')
get_new_direction 1 '^' '+' = (2, '^')
get_new_direction 2 '^' '+' = (0, '>')
get_new_direction 0 '<' '+' = (1, 'v')
get_new_direction 1 '<' '+' = (2, '<')
get_new_direction 2 '<' '+' = (0, '^')
get_new_direction 0 '>' '+' = (1, '^')
get_new_direction 1 '>' '+' = (2, '>')
get_new_direction 2 '>' '+' = (0, 'v')
get_new_direction turns old_direction piece =
  (turns, get_new_direction' old_direction piece)

step board pieces =
  map
    (\((x, y), turns, direction) ->
       let (dx, dy) = delta direction
           (x', y') = (x + dx, y + dy)
           (turns', direction') =
             get_new_direction turns direction (board !! y' !! x')
        in ((x', y'), turns', direction'))
    pieces

step2_helper _ [] p = p
step2_helper board (((x, y), turns, direction):remaining) done =
  let (dx, dy) = delta direction
      new_pos = (x + dx, y + dy)
      (turns', direction') =
        get_new_direction turns direction (board !! snd new_pos !! fst new_pos)
   in if new_pos `notElem` map piece_position remaining &&
         new_pos `notElem` map piece_position done
        then step2_helper board remaining ((new_pos, turns', direction') : done)
        else step2_helper
               board
               (filter ((/= new_pos) . piece_position) remaining)
               (filter ((/= new_pos) . piece_position) done)

step2 board pieces = step2_helper board (sort pieces) []

piece_position (p, _, _) = p

crash pieces = repeated $ map piece_position pieces

main = do
  contents <- readFile "./input13"
  let mappy = map (map field) $ lines contents
  let pieces =
        mapMaybe
          (\(pos, field) ->
             case piece field of
               Nothing -> Nothing
               Just f -> Just (pos, 0, f)) $
        concat $
        map
          (\(y, row) -> map (\(x, piece) -> ((x, y), piece)) $ zip [0 ..] row)
          (zip [0 ..] $ lines contents)
  print mappy
  print pieces
  print $ iterate (step mappy) pieces !! 1
  print $ iterate (step mappy) pieces !! 2
  print $ iterate (step mappy) pieces !! 3
  print $ iterate (step mappy) pieces !! 4
  print $ head $ filter (not . null) $ map crash $ iterate (step mappy) pieces
  print $
    head $ filter (\pieces -> length pieces <= 1) $ iterate (step2 mappy) pieces
