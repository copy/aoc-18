import Data.Foldable (toList)
import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (<|), (><), (|>))

step (e1, e2, board) =
  let k1 = fromJust $ Seq.lookup e1 board
      k2 = fromJust $ Seq.lookup e2 board
      t = k1 + k2
      new =
        Seq.fromList $
        if t < 10
          then [t]
          else [t `quot` 10, t `mod` 10]
      board' = board >< new
      e1' = (e1 + 1 + k1) `mod` Seq.length board'
      e2' = (e2 + 1 + k2) `mod` Seq.length board'
   in (e1', e2', board')

main = do
  let x = (0, 1, Seq.fromList [3, 7])
  print x
  print (iterate step x !! 1)
  print (iterate step x !! 2)
  print (iterate step x !! 3)
  print (iterate step x !! 4)
  print (iterate step x !! 5)
  print (iterate step x !! 6)
  print (iterate step x !! 7)
  print (iterate step x !! 8)
  print (iterate step x !! 9)
  let trd = (\(_, _, x) -> x)
  let k = 793031
  print $
    foldl (\a x -> a * 10 + x) 0 $
    take 10 $ drop k $ toList $ trd (iterate step x !! (k + 10))
  let k_as_seq =
        Seq.fromList $
        reverse $
        unfoldr
          (\d ->
             if d == 0
               then Nothing
               else Just (d `mod` 10, d `quot` 10))
          k
  print k_as_seq
  print $
    length $
    head $
    mapMaybe
      (\s ->
         let n = Seq.length k_as_seq
             (left0, right0) = Seq.splitAt (Seq.length s - n) s
             (left1, right1) = Seq.splitAt (Seq.length s - n - 1) s
          in if right0 == k_as_seq
               then Just left0
               else if Seq.take n right1 == k_as_seq
                      then Just left1
                      else Nothing) $
    map trd $ (iterate step x)
