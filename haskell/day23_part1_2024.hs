
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Graph = Map.Map String [String]

parseGraph :: String -> Graph
parseGraph input = foldl processLine Map.empty (lines input)
  where
    processLine g line =
      case span (/= '-') line of
        (a, '-':b) -> Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] g
        _          -> g

solve :: Graph -> Int
solve graph = length $ Set.fromList triplets
  where
    nodes = Map.keys graph
    adj g n = Map.findWithDefault [] n g
    
    isTargetTriplet (a, b, c) = any ("t" `isPrefixOf`) [a, b, c]
    triplets =
        [ triplet
        | a <- nodes, b <- nodes, c <- nodes
        , a < b && b < c
        , b `elem` adj graph a
        , c `elem` adj graph b
        , c `elem` adj graph a
        , isTargetTriplet (a,b,c)
        , let triplet = sort [a,b,c]
        ]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let graph = parseGraph input
  print (solve graph)
