
import System.IO

transform :: Int -> Int -> Int
transform subjectNumber loopSize = foldl (\acc _ -> (acc * subjectNumber) `mod` 20201227) 1 [1..loopSize]

findLoopSize :: Int -> Int
findLoopSize publicKey = go 1 0
  where
    go value loopSize
      | value == publicKey = loopSize
      | otherwise = go ((value * 7) `mod` 20201227) (loopSize + 1)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let [cardPublicKey, doorPublicKey] = map read $ lines contents
        cardLoopSize = findLoopSize cardPublicKey
        encryptionKey = transform doorPublicKey cardLoopSize
    print encryptionKey
    hClose handle
