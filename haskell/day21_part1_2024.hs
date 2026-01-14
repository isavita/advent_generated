
import qualified Data.Map.Strict as M
import Data.Char (isDigit, digitToInt)
import System.IO (readFile)

type Pad = [String]
type Pos = (Int, Int)

findPos :: Pad -> Char -> Pos
findPos pad ch = head [(i,j) | (i,row) <- zip [0..] pad
                             , (j,c)   <- zip [0..] row
                             , c == ch]

inBounds :: Pad -> Pos -> Bool
inBounds pad (i,j) = i >= 0 && i < length pad && j >= 0 && j < length (head pad)

ok :: Pad -> Pos -> String -> Bool
ok pad p = go p
  where
    go _ [] = True
    go (i,j) (c:cs)
      | not (inBounds pad (i,j)) = False
      | pad !! i !! j == ' '    = False
      | otherwise = let (i',j') = case c of
                                      '^' -> (i-1,j)
                                      'v' -> (i+1,j)
                                      '<' -> (i,j-1)
                                      '>' -> (i,j+1)
                                      _   -> (i,j)
                    in go (i',j') cs

genMoves :: Pos -> Char -> Pad -> String
genMoves (pi,pj) obj pad =
  let (oi,oj) = findPos pad obj
      first  = concat [replicate (pj-oj) '<' | pj>oj]
            ++ concat [replicate (pi-oi) '^' | pi>oi]
            ++ concat [replicate (oi-pi) 'v' | pi<oi]
            ++ concat [replicate (oj-pj) '>' | pj<oj]
  in if ok pad (pi,pj) first then first else
       concat [replicate (oj-pj) '>' | pj<oj]
          ++ concat [replicate (pi-oi) '^' | pi>oi]
          ++ concat [replicate (oi-pi) 'v' | pi<oi]
          ++ concat [replicate (pj-oj) '<' | pj>oj]

solve :: String -> Int -> Pad -> Pad -> Int -> M.Map (String,Int) Int -> (Int, M.Map (String,Int) Int)
solve code robots keyPad robotPad maxR memo
  | robots <= 0 = (length code, memo)
  | otherwise =
      case M.lookup (code,robots) memo of
        Just v  -> (v,memo)
        Nothing ->
          let startPos = if robots == maxR then (3,2) else (0,2)
              (total,m') = foldl step (0,memo) (zip code (scanl updatePos startPos code))
              res = total
          in (res, M.insert (code,robots) res m')
  where
    updatePos (pi,pj) ch = if robots == maxR then findPos keyPad ch else findPos robotPad ch
    step (acc,m) (ch,curPos) =
      let pad = if robots == maxR then keyPad else robotPad
          moves = genMoves curPos ch pad ++ "A"
          (sv,m2) = solve moves (robots-1) keyPad robotPad maxR m
      in (acc+sv,m2)

numericPart :: String -> Int
numericPart = foldl (\a c -> if isDigit c then a*10 + digitToInt c else a) 0

main :: IO ()
main = do
  content <- readFile "input.txt"
  let maxR = 3
      keyPad = ["789","456","123"," 0A"]
      robotPad = [" ^A","<v>"]
      codes = filter (not . null) . map (takeWhile (/='\r')) . lines $ content
      (ans,_) = foldl (\(tot,m) code ->
                let n = numericPart code
                    (sv,m') = solve code maxR keyPad robotPad maxR m
                in (tot + sv * n, m')
              ) (0, M.empty) codes
  print ans
