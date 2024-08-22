import System.IO
import Data.List
import Data.Char

main = do
    contents <- readFile "input.txt"
    let operations = lines contents
        initialPassword = "abcdefgh"
        scrambledPassword = foldl applyOperation initialPassword operations
    putStrLn scrambledPassword

applyOperation :: String -> String -> String
applyOperation password operation
    | "swap position" `isPrefixOf` operation = swapPosition password (read (words operation !! 2)) (read (words operation !! 5))
    | "swap letter" `isPrefixOf` operation = swapLetter password (head (words operation !! 2)) (head (words operation !! 5))
    | "rotate left" `isPrefixOf` operation = rotateLeft password (read (words operation !! 2))
    | "rotate right" `isPrefixOf` operation = rotateRight password (read (words operation !! 2))
    | "rotate based on position of letter" `isPrefixOf` operation = rotateBasedOnPosition password (head (words operation !! 6))
    | "reverse positions" `isPrefixOf` operation = reversePositions password (read (words operation !! 2)) (read (words operation !! 4))
    | "move position" `isPrefixOf` operation = movePosition password (read (words operation !! 2)) (read (words operation !! 5))
    | otherwise = password

swapPosition :: String -> Int -> Int -> String
swapPosition password x y = let (xVal, yVal) = (password !! x, password !! y)
                                password' = take x password ++ [yVal] ++ drop (x + 1) password
                            in take y password' ++ [xVal] ++ drop (y + 1) password'

swapLetter :: String -> Char -> Char -> String
swapLetter password x y = map (\c -> if c == x then y else if c == y then x else c) password

rotateLeft :: String -> Int -> String
rotateLeft password x = let n = length password
                            x' = x `mod` n
                        in drop x' password ++ take x' password

rotateRight :: String -> Int -> String
rotateRight password x = let n = length password
                             x' = x `mod` n
                         in drop (n - x') password ++ take (n - x') password

rotateBasedOnPosition :: String -> Char -> String
rotateBasedOnPosition password x = let index = case elemIndex x password of
                                            Just i -> i
                                            Nothing -> error "Character not found"
                                       rotations = 1 + index + if index >= 4 then 1 else 0
                                   in rotateRight password rotations

reversePositions :: String -> Int -> Int -> String
reversePositions password x y = let (start, middle, end) = (take x password, take (y - x + 1) (drop x password), drop (y + 1) password)
                                in start ++ reverse middle ++ end

movePosition :: String -> Int -> Int -> String
movePosition password x y = let (start, middle, end) = (take x password, password !! x, drop (x + 1) password)
                                password' = start ++ end
                            in take y password' ++ [middle] ++ drop y password'