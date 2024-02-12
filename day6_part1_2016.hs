
import Data.List

mostCommon :: String -> Char
mostCommon xs = head $ maximumBy (\x y -> compare (length x) (length y)) $ group $ sort xs

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    let message = map mostCommon $ transpose input
    putStrLn message
