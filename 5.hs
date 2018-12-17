import Data.Char (isAsciiLower, toLower, toUpper)

equal x y = toLower x == toLower y && isAsciiLower x /= isAsciiLower y

reduce [] = []
reduce [x] = [x]
reduce (x:resty@(y:rest))
  | equal x y = reduce rest
  | otherwise = x : reduce resty

fully_reduce x =
  let reduced = reduce x
   in if reduced == x
        then reduced
        else fully_reduce reduced

main = do
  contents <- readFile "./input5"
  let clean = filter (/= '\n') contents
  let result = fully_reduce $ clean
  print result
  print $ length result
  print $
    minimum $
    map
      (\to_remove ->
         length $
         fully_reduce $
         filter (\c -> c /= to_remove && c /= toUpper to_remove) clean)
      ['a' .. 'z']
