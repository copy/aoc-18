import qualified Data.HashMap.Strict as Map
import qualified Data.IntSet as Set
import Data.List (unfoldr)
import Data.List.Split (splitOneOf)
import qualified Data.Map.Strict as OMap
import Data.Maybe (fromJust)

main
  --let initial_text = "#..#.#..##......###...###"
 = do
  let initial_text =
        "#...##.#...#..#.#####.##.#..###.#.#.###....#...#...####.#....##..##..#..#..#..#.#..##.####.#.#.###"
  contents <- readFile "./input12"
  let rules =
        Map.fromList $
        map
          (\[[f0, f1, f2, f3, f4], to] ->
             ( (f0 == '#', f1 == '#', f2 == '#', f3 == '#', f4 == '#')
             , to == "#")) $
        map (filter (not . null) . splitOneOf " =>") $ lines contents
  print rules
  let initial =
        ( 0
        , Set.fromList
            (map fst $ filter ((== '#') . snd) $ zip [0 ..] initial_text))
  let step (offset, board) =
        let (min, max) = (Set.findMin board, Set.findMax board)
            new_board =
              Set.fromList
                (filter
                   (\i ->
                      let m j = Set.member j board
                          entry =
                            (m (i - 2), m (i - 1), m i, m (i + 1), m (i + 2))
                       in Map.lookup entry rules == Just True)
                   [min - 2 .. max + 2])
            new_min = Set.findMin new_board
         in (offset + new_min, Set.map (\x -> x - new_min) new_board)
  print initial
  print $ step initial
  print $ step $ step initial
  print $ step $ step $ step initial
  let (offset, twenty) = iterate step initial !! 20
  print twenty
  print $ Set.foldl (+) 0 $ Set.map (+ offset) twenty
  let (history, gen_new, board) =
        last $
        unfoldr
          (\(n, (offset, board), history) ->
             if OMap.member board history
               then Nothing
               else let history' = OMap.insert board n history
                        next = (n + 1, step (offset, board), history')
                     in Just ((history', n + 1, step (offset, board)), next))
          (0, initial, OMap.empty)
  print board
  let gen_old = fromJust $ OMap.lookup (snd board) history
  print ("gen old:", gen_old)
  print gen_new
  print (snd board == snd (iterate step initial !! gen_old))
  let offset_old = fst $ iterate step initial !! gen_old
  print ("offset old:", offset_old)
  let target_gen = 50000000000
  let offset_per_gen = 1
  let offset_50b = offset_old + offset_per_gen * (target_gen - gen_old)
  print $ Set.foldl (+) 0 $ Set.map (+ offset_50b) (snd board)
  --let (offset2, final) = iterate step initial !! 400000
  --print $ Set.foldl (+) 0 final
  --print $ Set.size final
  --print $ Set.findMin final
  --print $ Set.findMax final
