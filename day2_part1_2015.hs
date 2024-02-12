
import System.IO

main = do
    contents <- readFile "input.txt"
    let total = sum $ map calculateTotal $ lines contents
    print total

calculateTotal :: String -> Int
calculateTotal line = let [l, w, h] = map read $ wordsWhen (=='x') line
                          side1 = l * w
                          side2 = w * h
                          side3 = h * l
                          smallest = minimum [side1, side2, side3]
                      in 2*side1 + 2*side2 + 2*side3 + smallest

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
