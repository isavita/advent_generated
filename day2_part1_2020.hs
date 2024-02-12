import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let validPasswords = length . filter isValid . lines $ contents
    print validPasswords

isValid :: String -> Bool
isValid line =
    let (range, letter:_, password) = breakWords line
        (min, max) = parseRange range
        count = length . filter (== letter) $ password
    in count >= min && count <= max

breakWords :: String -> (String, String, String)
breakWords str =
    let [range, letter, password] = words str
    in (range, letter, password)

parseRange :: String -> (Int, Int)
parseRange str =
    let (min, _:max) = break (== '-') str
    in (read min, read max)
