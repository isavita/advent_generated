
import Data.Char (isDigit, digitToInt)

calc :: String -> Int
calc s = maximum $ 0 : [d1*10 + d2 | (i,c1) <- zip [0..] s,
                                    isDigit c1,
                                    let d1 = digitToInt c1,
                                    d2 <- [maximum $ -1 : map digitToInt (filter isDigit (drop (i+1) s))],
                                    d2 /= -1]

main :: IO ()
main = do
    txt <- readFile "input.txt"
    print $ sum $ map calc $ lines txt
