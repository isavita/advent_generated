
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import System.IO

type Component = (Int, Int)

maxStrength :: [Component] -> Int
maxStrength components = go 0 0 []
  where
    go port strength used = foldl' step strength components
      where
        step maxStr (a, b)
          | (a == port || b == port) && not (elem (a, b) used) =
              let nextPort = if a == port then b else a
                  newUsed = (a, b) : used
                  newStrength = strength + a + b
              in max maxStr (go nextPort newStrength newUsed)
          | otherwise = maxStr

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let components = map ((\[a, b] -> (read a, read b)) . splitOn '/') (lines contents)
    print $ maxStrength components

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim xs = let (before, remainder) = break (== delim) xs
                    in before : case remainder of
                                 [] -> []
                                 (_:after) -> splitOn delim after
