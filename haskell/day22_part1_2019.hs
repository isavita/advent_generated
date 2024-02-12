
import Data.List

size :: Int
size = 10007

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let commands = lines contents
        deck = [0..size-1]
        finalDeck = foldl' applyCommand deck commands
    print $ findCardPosition finalDeck 2019

applyCommand :: [Int] -> String -> [Int]
applyCommand deck "deal into new stack" = dealIntoNewStack deck
applyCommand deck ('c':'u':'t':' ':n) = cutN deck (read n)
applyCommand deck ('d':'e':'a':'l':' ':'w':'i':'t':'h':' ':'i':'n':'c':'r':'e':'m':'e':'n':'t':' ':n) = dealWithIncrement deck (read n)

dealIntoNewStack :: [Int] -> [Int]
dealIntoNewStack deck = reverse deck

cutN :: [Int] -> Int -> [Int]
cutN deck n
    | n >= 0 = drop n deck ++ take n deck
    | otherwise = drop (length deck + n) deck ++ take (length deck + n) deck

dealWithIncrement :: [Int] -> Int -> [Int]
dealWithIncrement deck n = map snd $ sortOn fst [(i * n `mod` size, card) | (i, card) <- zip [0..] deck]

findCardPosition :: [Int] -> Int -> Int
findCardPosition deck card = head [i | (i, c) <- zip [0..] deck, c == card]
