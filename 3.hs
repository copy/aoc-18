import qualified Data.List.Split as Split
import qualified Data.Map as Map

parse_list [id, x, y, width, height] =
  ( read id :: Int
  , read x :: Int
  , read y :: Int
  , read width :: Int
  , read height :: Int)
parse_list _ = error "bad"

rectangle_coords x y 0 height = []
rectangle_coords x y width height =
  zip (repeat x) (row y height) ++ rectangle_coords (x + 1) y (width - 1) height

row y height = enumFromTo y (y + height - 1)

main :: IO ()
main = do
  contents <- readFile "./input3"
  let l =
        map parse_list $
        map (filter (/= "")) $ map (Split.splitOneOf " x@#,:") $ lines contents
  let mappy =
        foldl
          (\mappy (_, offx, offy, w, h) ->
             foldl (\mappy2 pos -> Map.insertWith (+) pos 1 mappy2) mappy $
             rectangle_coords offx offy w h)
          Map.empty
          l
  putStrLn "foo"
  print l
  print $ rectangle_coords 3 4 5 9
    --print mappy
  print $
    foldl
      (\total coord_count ->
         total +
         if coord_count > 1
           then 1
           else 0)
      0
      mappy
  let mappy2 =
        foldl
          (\mappy (id, offx, offy, w, h) ->
             foldl (\mappy2 pos -> Map.insertWith (++) pos [id] mappy2) mappy $
             rectangle_coords offx offy w h)
          Map.empty
          l
  print $
    filter
      (\(id, offx, offy, w, h) ->
         all (\coord -> Map.lookup coord mappy2 == Just [id]) $
         rectangle_coords offx offy w h)
      l
