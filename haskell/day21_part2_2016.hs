
import System.IO
import Data.List
import Data.Maybe

type Password = String

swapPositions :: Int -> Int -> Password -> Password
swapPositions x y pw = pw'
  where
    pw' = take (min x y) pw ++ [pw !! (max x y)] ++ take (abs (x - y) - 1) (drop (min x y + 1) pw) ++ [pw !! (min x y)] ++ drop (max x y + 1) pw

swapLetters :: Char -> Char -> Password -> Password
swapLetters x y pw = swapPositions (fromJust $ elemIndex x pw) (fromJust $ elemIndex y pw) pw

rotate :: Int -> Password -> Password
rotate steps pw = let l = length pw in
                    let s = steps `mod` l in
                    drop (l - s) pw ++ take (l - s) pw

rotateLetter :: Char -> Password -> Password
rotateLetter x pw = let index = fromJust $ elemIndex x pw in
                      rotate (index + (if index >= 4 then 2 else 1)) pw

derotateLetter :: Char -> Password -> Password
derotateLetter x pw = let index = fromJust $ elemIndex x pw in
                         let rot = case index of
                                     1 -> -1
                                     3 -> -2
                                     5 -> -3
                                     7 -> -4
                                     0 -> -1
                                     2 -> 2
                                     4 -> 1
                                     6 -> 0
                         in rotate rot pw

reverseRange :: Int -> Int -> Password -> Password
reverseRange x y pw = take x pw ++ reverse (take (y - x + 1) (drop x pw)) ++ drop (y + 1) pw

move :: Int -> Int -> Password -> Password
move x y pw = let char = pw !! x in
                  let pw' = take x pw ++ drop (x + 1) pw in
                  take y pw' ++ [char] ++ drop y pw'

scramble :: [String] -> Password -> Password
scramble instructions pw = foldl' applyInstruction pw instructions
  where
    applyInstruction p instruction =
      let words = splitOn " " instruction in
      case head words of
        "swap" ->
          if words !! 1 == "position"
            then swapPositions (read (words !! 2) :: Int) (read (last words) :: Int) p
            else swapLetters (head (words !! 2)) (head (last words)) p
        "rotate" ->
          if words !! 1 == "based"
            then rotateLetter (last (head (reverse words))) p
            else let steps = read (words !! 2) :: Int in
                   if words !! 1 == "left"
                     then rotate (-steps) p
                     else rotate steps p
        "reverse" -> reverseRange (read (words !! 2) :: Int) (read (last words) :: Int) p
        "move" -> move (read (words !! 2) :: Int) (read (last words) :: Int) p
        _ -> error "Unknown instruction"

splitOn :: String -> String -> [String]
splitOn delim str = splitHelper delim str []
  where
    splitHelper _ "" acc = reverse ( "":acc )
    splitHelper delim str acc =
      case findSubstr delim str of
        Nothing -> reverse (str:acc)
        Just idx -> splitHelper delim (drop (idx + length delim) str) (take idx str : acc)

    findSubstr :: String -> String -> Maybe Int
    findSubstr delim str = findIndex (isPrefixOf delim) (tails str)

unscramble :: [String] -> Password -> Password
unscramble instructions pw = foldr applyInstruction pw instructions
  where
    applyInstruction instruction p =
      let words = splitOn " " instruction in
      case head words of
        "swap" ->
          if words !! 1 == "position"
            then swapPositions (read (words !! 2) :: Int) (read (last words) :: Int) p
            else swapLetters (head (words !! 2)) (head (last words)) p
        "rotate" ->
          if words !! 1 == "based"
            then derotateLetter (last (head (reverse words))) p
            else let steps = read (words !! 2) :: Int in
                   if words !! 1 == "left"
                     then rotate steps p
                     else rotate (-steps) p
        "reverse" -> reverseRange (read (words !! 2) :: Int) (read (last words) :: Int) p
        "move" -> move (read (last words) :: Int) (read (words !! 2) :: Int) p
        _ -> error "Unknown instruction"

main :: IO ()
main = do
  instructions <- fmap lines (readFile "input.txt")
  let hashed = "fbgdceah"
  let result = unscramble instructions hashed
  print result
