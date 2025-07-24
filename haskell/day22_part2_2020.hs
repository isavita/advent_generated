
import qualified Data.Set as S
import Data.List (intercalate)
import Data.Maybe (fromJust)

type Deck = [Int]

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse

play :: Deck -> Deck -> (Deck, Deck)
play = go S.empty
  where
    go seen p1 p2
      | null p1 || null p2 = (p1, p2)
      | (p1, p2) `S.member` seen = (p1, [])
      | otherwise =
          let seen' = S.insert (p1, p2) seen
              (c1:p1') = p1
              (c2:p2') = p2
              (winner1, winner2) =
                if length p1' >= c1 && length p2' >= c2
                then let (a, _) = play (take c1 p1') (take c2 p2')
                     in (not (null a), null a)
                else (c1 > c2, c2 > c1)
          in if winner1
             then go seen' (p1' ++ [c1, c2]) p2'
             else go seen' p1' (p2' ++ [c2, c1])

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let (p1, _:p2) = break null (drop 1 ls)
      deck1 = map read p1
      deck2 = map read (drop 1 p2)
      (w1, w2) = play deck1 deck2
      winner = if null w1 then w2 else w1
  print (score winner)
