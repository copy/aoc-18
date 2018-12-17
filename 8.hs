import Data.List.Split (splitOneOf)

data Tree =
  Node [Int]
       [Tree]
  deriving (Show)

to_tree (child_count:meta_count:rest) =
  let (children, rest') =
        foldl
          (\(cs, rest) _ ->
             let (child, rest') = to_tree rest
              in (child : cs, rest'))
          ([], rest)
          [1 .. child_count]
      (metadata, rest'') = splitAt meta_count rest'
   in ((Node metadata (reverse children)), rest'')

sum_m (Node m c) = sum m + (sum $ map sum_m c)

sum_2 (Node m []) = sum m
sum_2 (Node m c) =
  sum $
  map
    (\i ->
       if i >= 1 && i <= length c
         then sum_2 (c !! (i - 1))
         else 0)
    m

main = do
  contents <- readFile "./input8"
  --let contents = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
  let entries = map (\x -> read x :: Int) $ words contents
  let (root, rest) = to_tree entries
  print root
  print $ sum_m root
  print $ sum_2 root
