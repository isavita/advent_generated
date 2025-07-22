
import System.IO

main :: IO ()
main = readFile "input.txt" >>= putStr
