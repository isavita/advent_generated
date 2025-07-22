
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

type Color = String
type Rules = M.Map Color [(Int, Color)]

parse :: String -> Rules
parse = M.fromList . map (parseLine . words) . lines
  where
    parseLine (a:b:"bags":"contain":rest) = (a ++ " " ++ b, parseContains rest)
    parseContains ["no","other","bags."] = []
    parseContains (n:a:b:"bag,":rest)     = (read n, a ++ " " ++ b) : parseContains rest
    parseContains (n:a:b:"bags,":rest)   = (read n, a ++ " " ++ b) : parseContains rest
    parseContains (n:a:b:"bag.":_)       = [(read n, a ++ " " ++ b)]
    parseContains (n:a:b:"bags.":_)     = [(read n, a ++ " " ++ b)]

canContainGold :: Rules -> Color -> Bool
canContainGold rules = go S.empty
  where
    go seen c
      | c `S.member` seen = False
      | c == "shiny gold"  = True
      | otherwise = any (go (S.insert c seen) . snd) (fromMaybe [] $ M.lookup c rules)

countInside :: Rules -> Color -> Int
countInside rules = go
  where
    go c = sum [ n * (1 + go inner) | (n, inner) <- fromMaybe [] $ M.lookup c rules ]

main :: IO ()
main = do
  rules <- parse <$> readFile "input.txt"
  let part1 = length $ filter (canContainGold rules) $ filter (/= "shiny gold") $ M.keys rules
      part2 = countInside rules "shiny gold"
  print part1
  print part2
