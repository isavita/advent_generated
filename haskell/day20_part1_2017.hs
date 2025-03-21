
import System.IO
import Data.List
import Data.Maybe

manhattan :: (Int, Int, Int) -> Int
manhattan (x, y, z) = abs x + abs y + abs z

parseParticle :: String -> ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))
parseParticle line =
  let parts = splitBy ", " line
      p = parseCoords $ head parts
      v = parseCoords $ parts !! 1
      a = parseCoords $ parts !! 2
  in (p, v, a)

parseCoords :: String -> (Int, Int, Int)
parseCoords s =
  let coordsStr = splitBy "," $ drop 3 $ take (length s - 1) s
      coords = map read coordsStr :: [Int]
  in (coords !! 0, coords !! 1, coords !! 2)

splitBy :: String -> String -> [String]
splitBy delim str = splitBy' delim str []
  where
    splitBy' _ "" acc = reverse $ "":acc
    splitBy' delim str acc =
      case findSubstr delim str of
        Nothing -> reverse $ str:acc
        Just pos -> splitBy' delim (drop (pos + length delim) str) (take pos str:acc)

findSubstr :: String -> String -> Maybe Int
findSubstr needle haystack = findSubstr' needle haystack 0
  where
    findSubstr' _ "" _ = Nothing
    findSubstr' needle haystack idx
      | needle `isPrefixOf` haystack = Just idx
      | otherwise = findSubstr' needle (tail haystack) (idx + 1)

solve :: [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))] -> Int
solve particles =
  fst $ minimumBy compareParticles $ zip [0..] particles
  where
    compareParticles (i1, (_, _, a1)) (i2, (_, _, a2)) = compare (manhattan a1) (manhattan a2)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines' = lines contents
      particles = map parseParticle lines'
      result = solve particles
  print result
