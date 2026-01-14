
import System.IO
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.List (foldl')

target :: Int
target = 12

process :: String -> Maybe Integer
process line =
    let s = reverse $ dropWhile (not . isDigit) $ reverse line
    in if length s < target then Nothing else
        let rem0 = length s - target
            (remFinal, stack) = foldl' step (rem0, []) s
            step (rem, stk) c =
                let (rem', stk') = popWhile rem stk c
                in (rem', stk' ++ [c])
            popWhile r xs c
                | r > 0 && not (null xs) && last xs < c = popWhile (r - 1) (init xs) c
                | otherwise = (r, xs)
            numStr = take target stack
        in Just (read numStr)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let total = sum $ mapMaybe process (lines contents)
    print total
