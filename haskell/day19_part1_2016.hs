
import System.IO

main :: IO ()
main = do
    totalElves <- readInput "input.txt"
    let winner = findWinningElf totalElves
    print winner

readInput :: FilePath -> IO Int
readInput filename = do
    handle <- openFile filename ReadMode
    totalElvesStr <- hGetLine handle
    let totalElves = read totalElvesStr :: Int
    hClose handle
    return totalElves

findWinningElf :: Int -> Int
findWinningElf totalElves = (totalElves - highestPowerOfTwo) * 2 + 1
    where highestPowerOfTwo = last $ takeWhile (<= totalElves) $ iterate (*2) 1
