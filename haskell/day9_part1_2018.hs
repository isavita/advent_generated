
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), (><))
import Data.Foldable (toList)
import Data.List (maximumBy)
import Data.Ord (comparing)

parseInput :: String -> (Int, Int)
parseInput s = (read players, read lastMarble)
  where
    ws = words s
    players = head ws
    lastMarble = ws !! 6

rotate :: Int -> Seq a -> Seq a
rotate n s
  | Seq.null s = s
  | otherwise =
    let
      len = Seq.length s
      actualRotation = n `mod` len
    in
      case actualRotation of
        0 -> s
        x | x > 0 ->
          let (left, right) = Seq.splitAt (len - x) s
          in  right >< left
        x ->
          let (left, right) = Seq.splitAt (abs x) s
          in right >< left

playMarbleGame :: Int -> Int -> Int
playMarbleGame players lastMarble = maximum $ map snd $ toList $ go 1 (Seq.singleton (0,0)) (Seq.fromList (zip [0..players-1] (repeat 0)))
    where
      go marble circle scores
        | marble > lastMarble = scores
        | marble `mod` 23 == 0 =
          let
            rotatedCircle = rotate 7 circle
            (currentPlayer, currentScore) = Seq.index scores (marble `mod` players)
            (removedMarble, newCircle) = case Seq.viewr rotatedCircle of
                Seq.EmptyR -> error "should not happen"
                xs Seq.:> x -> (x, xs)

            newScores = Seq.update (marble `mod` players) (currentPlayer, currentScore + marble + fst removedMarble) scores
            nextCircle = rotate (-1) newCircle
          in
            go (marble + 1) nextCircle newScores
        | otherwise =
          let
            rotatedCircle = rotate (-1) circle
            newCircle = rotatedCircle |> (marble,0)
          in go (marble + 1) newCircle scores

main :: IO ()
main = do
  content <- readFile "input.txt"
  let (players, lastMarble) = parseInput content
  print $ playMarbleGame players lastMarble
