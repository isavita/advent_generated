
import qualified Data.Map.Strict as M

data Rule = Rule Char Char Int String | Direct String

type Workflow = [Rule]
type Ranges = M.Map Char (Int, Int)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s = case break p s of
    (w, [])     -> [w]
    (w, _:rest) -> w : splitBy p rest

parseRule :: String -> Rule
parseRule r = case break (== ':') r of
    (cond, ':':dest) -> Rule (head cond) (cond !! 1) (read (drop 2 cond)) dest
    (dest, _)        -> Direct dest

parseWorkflow :: String -> (String, Workflow)
parseWorkflow s = 
    let (name, rest) = break (== '{') s
        rulesStr = init (tail rest)
    in (name, map parseRule (splitBy (== ',') rulesStr))

countCombos :: Ranges -> Integer
countCombos rs = product [fromIntegral (max 0 (hi - lo + 1)) | (lo, hi) <- M.elems rs]

solve :: M.Map String Workflow -> String -> Ranges -> Integer
solve _ "A" rs = countCombos rs
solve _ "R" _  = 0
solve ws name rs = go (ws M.! name) rs
  where
    go [] _ = 0
    go (Direct dest : _) currRs = solve ws dest currRs
    go (Rule v op val dest : rest) currRs =
        let (lo, hi) = currRs M.! v
            (trueR, falseR) = if op == '<'
                then ((lo, min hi (val - 1)), (max lo val, hi))
                else ((max lo (val + 1), hi), (lo, min hi val))
            tPart = if fst trueR <= snd trueR
                    then solve ws dest (M.insert v trueR currRs)
                    else 0
            fPart = if fst falseR <= snd falseR
                    then go rest (M.insert v falseR currRs)
                    else 0
        in tPart + fPart

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let workflowLines = takeWhile (not . null) (lines contents)
        workflows = M.fromList $ map parseWorkflow workflowLines
        initialRanges = M.fromList [('x', (1, 4000)), ('m', (1, 4000)), ('a', (1, 4000)), ('s', (1, 4000))]
    print $ solve workflows "in" initialRanges

