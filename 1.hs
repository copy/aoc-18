import qualified Data.Set as Set

mapper :: String -> Int
mapper x =
  if head x == '+'
    then read (tail x)
    else read x :: Int

main :: IO ()
main = do
  contents <- readFile "input1"
  let values = map mapper $ lines contents
  let partial_sums = scanl1 (+) (take 1000000 $ cycle $ cycle values)
  let (_, seen_twice2) =
        foldl
          (\(seen, seen_twice) x ->
             if x `Set.member` seen
               then (seen, x : seen_twice)
               else (Set.insert x seen, seen_twice))
          (Set.empty, [])
          partial_sums
  print $ sum values
    --putStrLn $ show partial_sums
  putStrLn "----"
  print $ last seen_twice2
