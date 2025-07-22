
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (when)

main :: IO ()
main = do
  nums <- fmap (map read . lines) (readFile "input.txt") :: IO [Int]
  print $ solve1 nums
  print $ solve2 nums

solve1 :: [Int] -> Int
solve1 xs = runST $ do
  arr <- newListArray (0, length xs - 1) xs :: ST s (STUArray s Int Int)
  let go !i !steps
        | i < 0 || i >= length xs = return steps
        | otherwise = do
            offset <- readArray arr i
            writeArray arr i (offset + 1)
            go (i + offset) (steps + 1)
  go 0 0

solve2 :: [Int] -> Int
solve2 xs = runST $ do
  arr <- newListArray (0, length xs - 1) xs :: ST s (STUArray s Int Int)
  let go !i !steps
        | i < 0 || i >= length xs = return steps
        | otherwise = do
            offset <- readArray arr i
            let delta = if offset >= 3 then -1 else 1
            writeArray arr i (offset + delta)
            go (i + offset) (steps + 1)
  go 0 0
