
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Sequence (Seq (..), (<|), (|>))
import System.IO (readFile)

main :: IO ()
main = do
  txt <- readFile "input.txt"
  let target = map (read . (:[])) $ init $ filter (/= '\n') txt
      tlen   = length target
      go sb e1 e2 = 
        let s1 = S.index sb e1
            s2 = S.index sb e2
            ns = s1 + s2
            (sb', n) = if ns >= 10 then (sb |> (ns `div` 10) |> (ns `mod` 10), 2) else (sb |> (ns `mod` 10), 1)
            len = S.length sb'
            check = len >= tlen && (F.toList $ S.take tlen $ S.drop (len - tlen) sb') == target
            e1' = (e1 + s1 + 1) `mod` len
            e2' = (e2 + s2 + 1) `mod` len
        in if check then len - tlen else go sb' e1' e2'
  print $ go (S.fromList [3,7]) 0 1
