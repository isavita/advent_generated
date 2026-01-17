import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (isPrefixOf)

data Rule = Literal String | SubRules [[Int]]

splitOn :: String -> String -> [String]
splitOn _ "" = []
splitOn delim s = case breakOn delim s of
    (pre, "") -> [pre]
    (pre, post) -> pre : splitOn delim (drop (length delim) post)
  where
    breakOn d str | d `isPrefixOf` str = ("", str)
                  | null str = (str, "")
                  | otherwise = let (a, b) = breakOn d (tail str) in (head str : a, b)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n s = take n s : chunksOf n (drop n s)

parseLine :: String -> (Int, Rule)
parseLine s =
    let (left, right) = break (== ':') s
        idx = read left
        rest = drop 2 right
    in if not (null rest) && head rest == '"'
       then (idx, Literal [rest !! 1])
       else (idx, SubRules $ map (map read . words) (splitOn " | " rest))

main :: IO ()
main = do
    content <- readFile "input.txt"
    let parts = splitOn "\n\n" content
    if length parts < 2 then return () else do
        let rulesPart = head parts
            messagesPart = parts !! 1
            rulesMap = M.fromList $ map parseLine (lines rulesPart)
            memo = M.fromList [(i, expand i) | i <- M.keys rulesMap]
            expand i = case M.lookup i rulesMap of
                Just (Literal s) -> [s]
                Just (SubRules opts) -> concatMap (\opt -> foldl (\acc ri -> [a ++ b | a <- acc, b <- memo M.! ri]) [""] opt) opts
                Nothing -> []
            s42List = M.findWithDefault [] 42 memo
            s31List = M.findWithDefault [] 31 memo
            s42 = S.fromList s42List
            s31 = S.fromList s31List
            chunkLen = if null s42List then 0 else length (head s42List)
            isValid msg =
                let len = length msg
                    chunks = if chunkLen == 0 then [] else chunksOf chunkLen msg
                    (c42, rest) = span (`S.member` s42) chunks
                    (c31, final) = span (`S.member` s31) rest
                    n42 = length c42
                    n31 = length c31
                in chunkLen > 0 && len `rem` chunkLen == 0 && null final && n31 >= 1 && n42 > n31
        print $ length $ filter isValid (lines messagesPart)