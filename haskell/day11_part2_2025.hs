
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Control.Monad.State.Strict
import           System.IO
import           Data.List (words, dropWhileEnd)
import           Data.Char (isSpace)

type Adj  = Map String [String]
type Memo = Map String Integer

-- trim whitespace (both ends)
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- parse the input file into an adjacency map
readAdj :: FilePath -> IO Adj
readAdj fp = do
    content <- readFile fp
    let ls = filter (not . null) $ map trim $ lines content
        addLine mp line =
            let (srcPart, rest) = break (== ':') line
                src  = trim srcPart
                dest = map trim $ words $ drop 1 rest   -- drop ':' then split
            in M.insert src dest mp
    return $ foldl addLine M.empty ls

-- depth‑first search with memoisation (target = finish)
dfs :: Adj -> String -> String -> State Memo Integer
dfs adj cur fin
    | cur == fin = return 1
    | otherwise  = do
        cache <- get
        case M.lookup cur cache of
          Just v  -> return v
          Nothing -> do
              let neigh = M.findWithDefault [] cur adj
              vals <- mapM (\n -> dfs adj n fin) neigh
              let total = sum vals
              modify (M.insert cur total)
              return total

countPaths :: Adj -> String -> String -> Integer
countPaths adj s f = evalState (dfs adj s f) M.empty

main :: IO ()
main = do
    adj <- readAdj "input.txt"
    let s1 = countPaths adj "svr" "dac"
           * countPaths adj "dac" "fft"
           * countPaths adj "fft" "out"
        s2 = countPaths adj "svr" "fft"
           * countPaths adj "fft" "dac"
           * countPaths adj "dac" "out"
    putStrLn $ "Paths (svr->dac->fft->out): " ++ show s1
    putStrLn $ "Paths (svr->fft->dac->out): " ++ show s2
    putStrLn $ "Total paths visiting both: " ++ show (s1 + s2)
