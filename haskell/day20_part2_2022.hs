
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Foldable as F
import Data.Maybe (fromJust)

type Element = (Int, Integer)
type Grove = Seq Element

parseInput :: String -> [Integer]
parseInput = map read . lines

mixStep :: Grove -> Int -> Grove
mixStep grove originalIndex =
  let n = Seq.length grove
      n_minus_1 = fromIntegral (n - 1)
      Just currentPos = Seq.findIndexL (\(oi, _) -> oi == originalIndex) grove
      element@(_, val) = Seq.index grove currentPos
  in if n <= 1 || val == 0 then grove
     else
       let
         removedGrove = Seq.deleteAt currentPos grove
         newPosInteger = (fromIntegral currentPos + val)
         -- Python's % behavior: ((a % n) + n) % n
         newPos = fromIntegral $ (newPosInteger `mod` n_minus_1 + n_minus_1) `mod` n_minus_1
       in Seq.insertAt newPos element removedGrove

mixRound :: Grove -> Grove
mixRound initialGrove = foldl mixStep initialGrove [0 .. Seq.length initialGrove - 1]

coords :: Grove -> Integer
coords finalGrove =
  let n = Seq.length finalGrove
      Just zeroPos = Seq.findIndexL (\(_, v) -> v == 0) finalGrove
      getVal offset = snd $ Seq.index finalGrove ((zeroPos + offset) `mod` n)
  in getVal 1000 + getVal 2000 + getVal 3000

solve :: String -> Integer
solve input =
  let nums = parseInput input
      n = length nums
      initialGrove = Seq.fromList $ zip [0..] nums
      decryptionKey = 811589153
      grovePart2 = fmap (\(idx, val) -> (idx, val * decryptionKey)) initialGrove
      mixed10Times = iterate mixRound grovePart2 !! 10
  in coords mixed10Times

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ solve input
