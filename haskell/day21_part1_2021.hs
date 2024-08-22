import System.IO

main = do
    contents <- readFile "input.txt"
    let [p1Start, p2Start] = map (read . last . words) (lines contents)
        scores = [0, 0]
        positions = [p1Start, p2Start]
        diceRolls = 0
        currentPlayer = 0
        dice = cycle [1..100]
        (finalScores, totalRolls) = playGame scores positions diceRolls currentPlayer dice
        losingScore = minimum finalScores
    print (losingScore * totalRolls)

playGame :: [Int] -> [Int] -> Int -> Int -> [Int] -> ([Int], Int)
playGame scores positions diceRolls currentPlayer dice
    | any (>= 1000) scores = (scores, diceRolls)
    | otherwise = playGame newScores newPositions newDiceRolls newCurrentPlayer newDice
    where
        rollSum = sum (take 3 dice)
        newDiceRolls = diceRolls + 3
        newPosition = (positions !! currentPlayer + rollSum - 1) `mod` 10 + 1
        newScores = updateList scores currentPlayer ((scores !! currentPlayer) + newPosition)
        newPositions = updateList positions currentPlayer newPosition
        newCurrentPlayer = (currentPlayer + 1) `mod` 2
        newDice = drop 3 dice

updateList :: [a] -> Int -> a -> [a]
updateList xs index newValue = take index xs ++ [newValue] ++ drop (index + 1) xs