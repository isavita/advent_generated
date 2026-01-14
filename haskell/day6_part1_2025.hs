import Data.Char (isSpace)
import Data.List (foldl')

isSeparator :: Int -> [String] -> Bool
isSeparator col ls = all (\ln -> col >= length ln || isSpace (ln !! col)) ls

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

blockResult :: Int -> Int -> [String] -> Int
blockResult sc ec ls =
    let (op, numsRev) = foldl' step (0, []) ls
        nums = reverse numsRev
    in if null nums
          then 0
          else case op of
                 1 -> sum nums
                 2 -> product nums
                 _ -> head nums
  where
    step (opAcc, ns) ln =
        let seg = trim $ take (ec - sc + 1) $ drop sc ln
        in if null seg
              then (opAcc, ns)
              else case seg of
                     "+" -> (1, ns)
                     "*" -> (2, ns)
                     _   -> (opAcc, read seg : ns)

getBlocks :: [String] -> Int -> [(Int, Int)]
getBlocks ls maxW = reverse $ go 0 False 0 []
  where
    go col inBlock start acc
        | col == maxW =
            if inBlock then (start, maxW - 1) : acc else acc
        | not (isSeparator col ls) =
            if inBlock then go (col + 1) True start acc
                       else go (col + 1) True col acc
        | otherwise =
            if inBlock then go (col + 1) False 0 ((start, col - 1) : acc)
                       else go (col + 1) False 0 acc

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
        maxW = maximum (0 : map length ls)
        blocks = getBlocks ls maxW
        grand = sum $ map (\(s, e) -> blockResult s e ls) blocks
    putStrLn $ "Grand total: " ++ show grand