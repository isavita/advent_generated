
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)

offset :: Integer
offset = 10000000000000

parseCoords :: String -> Maybe (Integer, Integer)
parseCoords s = case span (/= ',') s of
    (a, _:b) -> do
        x <- readMaybe (filter (\c -> isDigit c || c == '-') a)
        y <- readMaybe (filter (\c -> isDigit c || c == '-') b)
        return (x, y)
    _ -> Nothing

solveMachine :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Maybe Integer
solveMachine ax ay bx by px py =
    let d = ax * by - ay * bx
        numA = px * by - py * bx
        numB = py * ax - px * ay
    in if d == 0 then Nothing
       else if numA `mod` d /= 0 || numB `mod` d /= 0 then Nothing
       else let a = numA `div` d
                b = numB `div` d
            in if a < 0 || b < 0 then Nothing
               else Just (3 * a + b)

processLines :: [String] -> (Integer, Integer)
processLines = go 0 0 0 0 0 0 0 0
  where
    go total solved ax ay bx by px py [] =
        let px' = px + offset
            py' = py + offset
        in case solveMachine ax ay bx by px' py' of
            Just cost -> (total + cost, solved + 1)
            Nothing -> (total, solved)
    go total solved ax ay bx by px py (l:ls)
        | null (words l) =
            let px' = px + offset
                py' = py + offset
                (newTotal, newSolved) = case solveMachine ax ay bx by px' py' of
                    Just cost -> (total + cost, solved + 1)
                    Nothing -> (total, solved)
            in go newTotal newSolved 0 0 0 0 0 0 ls
        | "Button A:" `isPrefixOf` l || "A:" `isPrefixOf` l =
            case parseCoords (dropWhile (/= ':') l) of
                Just (x, y) -> go total solved x y bx by px py ls
                Nothing -> go total solved ax ay bx by px py ls
        | "Button B:" `isPrefixOf` l || "B:" `isPrefixOf` l =
            case parseCoords (dropWhile (/= ':') l) of
                Just (x, y) -> go total solved ax ay x y px py ls
                Nothing -> go total solved ax ay bx by px py ls
        | "Prize:" `isPrefixOf` l || "P:" `isPrefixOf` l =
            case parseCoords (dropWhile (/= ':') l) of
                Just (x, y) -> go total solved ax ay bx by x y ls
                Nothing -> go total solved ax ay bx by px py ls
        | otherwise = go total solved ax ay bx by px py ls

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (total, solved) = processLines (lines content)
    putStrLn $ show solved ++ " " ++ show total
