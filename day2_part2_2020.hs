import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let validPasswords = length . filter isValid . lines $ contents
    print validPasswords

isValid :: String -> Bool
isValid line =
    let (positions, letter:_, password) = breakWords line
        (pos1, pos2) = parsePositions positions
        letterAtPos1 = password !! (pos1 - 1)
        letterAtPos2 = password !! (pos2 - 1)
    in (letter == letterAtPos1) /= (letter == letterAtPos2)

breakWords :: String -> (String, String, String)
breakWords str =
    let [positions, letter, password] = words str
    in (positions, letter, password)

parsePositions :: String -> (Int, Int)
parsePositions str =
    let (pos1, _:pos2) = break (== '-') str
    in (read pos1, read pos2)
