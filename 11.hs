import Control.Exception.Base (assert)
import Data.Function (fix)
import Data.List (maximumBy)
import qualified Data.Map.Lazy as Map
import Data.MemoTrie (memoFix)
import Data.Ord (comparing)

input = 7139 :: Int

power (x, y) =
  let q = (x + 10) * (y * (x + 10) + input)
   in (q `quot` 100) `mod` 10 - 5

powern p (x, y, (1, 1)) = power (x, y)
powern p (x, y, (1, m)) = power (x, y) + p (x, y + 1, (1, m - 1))
powern p (x, y, (n, 1)) = power (x, y) + p (x + 1, y, (n - 1, 1))
powern p (x, y, (n, m)) =
  assert (n == m) $
  p (x, y, (n - 1, n - 1)) + p (x + n - 1, y, (1, n)) +
  p (x, y + n - 1, (n - 1, 1))

fastpowern = memoFix powern

main = do
  print $ power (3, 5)
  print $
    maximumBy
      (comparing fastpowern)
      [(x, y, (n, n)) | x <- [1 .. 300 - 2], y <- [1 .. 300 - 2], n <- [3]]
  print $
    maximumBy
      (comparing fastpowern)
      [ (x, y, (n, n))
      | n <- [1 .. 50]
      , x <- [1 .. 300 - n]
      , y <- [1 .. 300 - n]
      ]
