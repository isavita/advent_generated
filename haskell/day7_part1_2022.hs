
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (foldl', inits, intercalate, isPrefixOf)
import Data.Char (isDigit)
import System.IO (readFile)

type Path = [String]
type DirSizes = Map String Int

pathToKey :: Path -> String
pathToKey [] = "/"
pathToKey ps = "/" ++ intercalate "/" ps

processLine :: (Path, DirSizes) -> String -> (Path, DirSizes)
processLine (path, sizes) line
    | "$ cd /"  `isPrefixOf` line = ([], sizes)
    | "$ cd .." `isPrefixOf` line = (if null path then [] else init path, sizes)
    | "$ cd "   `isPrefixOf` line = let dir = drop 5 line in (path ++ [dir], sizes)
    | "$ ls"    `isPrefixOf` line = (path, sizes)
    | "dir "    `isPrefixOf` line = (path, sizes)
    | otherwise                   =
        case words line of
            [sizeStr, _] | all isDigit sizeStr ->
                let size = read sizeStr :: Int
                    pathsToUpdate = inits path
                    keysToUpdate = map pathToKey pathsToUpdate
                    updateFn s = foldl' (\acc k -> Map.insertWith (+) k size acc) s keysToUpdate
                in (path, updateFn sizes)
            _ -> (path, sizes)

parse :: [String] -> DirSizes
parse = snd . foldl' processLine ([], Map.empty)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let commands = lines contents
    let allDirSizes = parse commands
    let relevantSizes = Map.filter (<= 100000) allDirSizes
    print $ sum $ Map.elems relevantSizes

