
import qualified Data.Map as M
import Data.Char (isDigit)
import Data.List (foldl')

type Pos = (Int, Int)
type Memo = M.Map (String, Int) Integer

keyPad :: Char -> Pos
keyPad '7' = (0,0); keyPad '8' = (0,1); keyPad '9' = (0,2)
keyPad '4' = (1,0); keyPad '5' = (1,1); keyPad '6' = (1,2)
keyPad '1' = (2,0); keyPad '2' = (2,1); keyPad '3' = (2,2)
keyPad '0' = (3,1); keyPad 'A' = (3,2); keyPad _ = (3,0)

robotPad :: Char -> Pos
robotPad '^' = (0,1); robotPad 'A' = (0,2)
robotPad '<' = (1,0); robotPad 'v' = (1,1); robotPad '>' = (1,2)
robotPad _ = (0,0)

ok :: (Char -> Pos) -> Pos -> String -> Bool
ok pad pos seq = go pos seq
  where
    gap = pad ' '
    go _ [] = True
    go (r, c) (s:ss) =
      let next = case s of
            '^' -> (r-1, c)
            'v' -> (r+1, c)
            '<' -> (r, c-1)
            '>' -> (r, c+1)
            _   -> (r, c)
      in next /= gap && go next ss

generateMoves :: (Char -> Pos) -> Pos -> Char -> String
generateMoves pad (r1, c1) target =
  let (r2, c2) = pad target
      dr = r2 - r1
      dc = c2 - c1
      hLt = replicate (max 0 (-dc)) '<'
      vUp = replicate (max 0 (-dr)) '^'
      vDn = replicate (max 0 dr) 'v'
      hRt = replicate (max 0 dc) '>'
      res1 = hLt ++ vUp ++ vDn ++ hRt
      res2 = hRt ++ vUp ++ vDn ++ hLt
  in if ok pad (r1, c1) res1 then res1 else res2

solve :: String -> Int -> Int -> Memo -> (Integer, Memo)
solve code robots maxRobots memo
  | robots <= 0 = (fromIntegral $ length code, memo)
  | M.member (code, robots) memo = (memo M.! (code, robots), memo)
  | otherwise =
      let pad = if robots == maxRobots then keyPad else robotPad
          (finalLen, finalMemo, _) = foldl' (\(totalLen, currMemo, currChar) char ->
              let moves = generateMoves pad (pad currChar) char ++ "A"
                  (subLen, nextMemo) = solve moves (robots - 1) maxRobots currMemo
              in (totalLen + subLen, nextMemo, char)
            ) (0, memo, 'A') code
          newMemo = M.insert (code, robots) finalLen finalMemo
      in (finalLen, newMemo)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let codes = filter (not . null) . map (filter (/= '\r')) . lines $ content
      maxRobots = 26
      (ans, _) = foldl' (\(grandTotal, currMemo) code ->
          let numericPart = read (filter isDigit code) :: Integer
              (len, nextMemo) = solve code maxRobots maxRobots currMemo
          in (grandTotal + len * numericPart, nextMemo)
        ) (0, M.empty) codes
  print ans

