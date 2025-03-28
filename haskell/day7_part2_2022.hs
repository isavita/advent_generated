
import System.IO
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (foldl')

-- Data Types --

-- Represents either a file with a size or a directory marker.
data Item = FileItem Int | DirItem deriving (Show, Eq)

-- Represents the contents of a directory: a map from names (String) to Items.
type DirectoryContents = Map.Map String Item

-- Represents the entire filesystem: a map from paths ([String]) to DirectoryContents.
-- Example path: ["/", "a", "e"]
type FileSystem = Map.Map [String] DirectoryContents

-- Represents the current path within the filesystem.
type CurrentPath = [String]

-- Map to store calculated directory sizes for memoization.
type SizeMap = Map.Map [String] Int

-- Parsing Logic --

-- Processes a list of terminal output lines to build the FileSystem representation.
buildFileSystem :: [String] -> FileSystem
buildFileSystem lines = go lines ["/"] (Map.singleton ["/"] Map.empty) -- Start at root, ensure root exists
  where
    go :: [String] -> CurrentPath -> FileSystem -> FileSystem
    go [] _ fs = fs -- End of input
    go (line:rest) currentPath fs =
        case words line of
            -- Change Directory commands
            ["$", "cd", "/"]    -> go rest ["/"] fs
            ["$", "cd", ".."]   -> let newPath = if length currentPath > 1 then init currentPath else ["/"] -- Avoid going above root
                                   in go rest newPath fs
            ["$", "cd", dirName] -> let newPath = currentPath ++ [dirName]
                                        -- Ensure the new directory path exists in the map, even if empty for now
                                        fs' = Map.insertWith (\_ oldVal -> oldVal) newPath Map.empty fs
                                    in go rest newPath fs'
            -- List command: process subsequent lines as directory content
            ["$", "ls"]         -> let (newFs, remainingLines) = processLsOutput rest currentPath fs
                                   in go remainingLines currentPath newFs
            -- Handle unexpected lines defensively (shouldn't occur with valid input)
            _                   -> error ("Unexpected command or format: " ++ line)

    -- Processes the output of an 'ls' command until the next command or end of input.
    processLsOutput :: [String] -> CurrentPath -> FileSystem -> (FileSystem, [String])
    processLsOutput [] currentPath fs = (fs, []) -- End of input during ls
    processLsOutput (line:rest) currentPath fs =
        case words line of
            -- Next command found, return current FS and remaining lines
            ("$":_) -> (fs, line:rest)
            -- Directory entry
            ["dir", dirName] ->
                let updatedContents itemMap = Map.insert dirName DirItem itemMap
                    newFs = Map.adjust updatedContents currentPath fs
                 in processLsOutput rest currentPath newFs
            -- File entry
            [sizeStr, fileName] ->
                 case reads sizeStr of
                     [(size, "")] ->
                         let updatedContents itemMap = Map.insert fileName (FileItem size) itemMap
                             newFs = Map.adjust updatedContents currentPath fs
                          in processLsOutput rest currentPath newFs
                     _ -> error ("Invalid file size format: " ++ line)
            -- Handle empty lines or other unexpected formats within ls output
            _  -> processLsOutput rest currentPath fs -- Skip potentially empty/malformed lines


-- Size Calculation Logic --

-- Calculates the total size for every directory, using memoization.
calculateAllSizes :: FileSystem -> SizeMap
calculateAllSizes fs = fst $ Map.foldlWithKey' computeSizeForAllPaths (Map.empty, fs) fs
  where
    -- Helper to ensure computeSize is called for each path in the initial FileSystem keys.
    computeSizeForAllPaths :: (SizeMap, FileSystem) -> [String] -> DirectoryContents -> (SizeMap, FileSystem)
    computeSizeForAllPaths (sizeMap, fileSystem) path _ =
        let (finalSizeMap, _) = computeSize path fileSystem sizeMap
         in (finalSizeMap, fileSystem) -- Pass the original fileSystem, update only the sizeMap

-- Computes the size of a single directory specified by path, using memoization (sizeMap).
-- Returns the updated sizeMap and the calculated size for the given path.
computeSize :: [String] -> FileSystem -> SizeMap -> (SizeMap, Int)
computeSize path fileSystem sizeMap =
    case Map.lookup path sizeMap of
        Just size -> (sizeMap, size) -- Return memoized size if available
        Nothing ->
            -- Size not computed yet, calculate it
            let contents = fromMaybe Map.empty (Map.lookup path fileSystem)
                -- Fold over items in the directory, accumulating size and updating sizeMap
                (updatedSizeMap, totalSize) = Map.foldlWithKey' accumulateItemSize (sizeMap, 0) contents

                accumulateItemSize :: (SizeMap, Int) -> String -> Item -> (SizeMap, Int)
                accumulateItemSize (currentSizeMap, currentDirSize) itemName item =
                    case item of
                        FileItem size -> (currentSizeMap, currentDirSize + size) -- Add file size directly
                        DirItem ->
                            let subDirPath = path ++ [itemName]
                                -- Recursively compute size of subdirectory
                                (newSizeMap, subDirSize) = computeSize subDirPath fileSystem currentSizeMap
                             in (newSizeMap, currentDirSize + subDirSize) -- Add subdirectory size

            -- Memoize the calculated size before returning
             in (Map.insert path totalSize updatedSizeMap, totalSize)


-- Puzzle Logic --

-- Part 1: Sum of sizes of directories with total size <= 100000
part1 :: SizeMap -> Int
part1 sizeMap = Map.foldr' (\size acc -> if size <= 100000 then acc + size else acc) 0 sizeMap

-- Part 2: Find the smallest directory size >= required space to free up
part2 :: SizeMap -> Int
part2 sizeMap =
    let totalDiskSpace = 70000000
        requiredUnusedSpace = 30000000
        -- Root directory size is the total used space
        usedSpace = fromMaybe (error "Root directory ['/'] size not found in SizeMap") (Map.lookup ["/"] sizeMap)
        currentFreeSpace = totalDiskSpace - usedSpace
        neededSpace = requiredUnusedSpace - currentFreeSpace
        -- We only need to free up space if neededSpace > 0
        minSpaceToFree = max 0 neededSpace

        -- Find all directories large enough
        candidateSizes = Map.filter (>= minSpaceToFree) sizeMap

    in if Map.null candidateSizes
       then error "No directory large enough to free required space"
       -- Find the minimum size among candidates
       else minimum (Map.elems candidateSizes)

-- Main Entry Point --

main :: IO ()
main = do
    -- Read input from file
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let inputLines = lines contents
    -- Build the filesystem representation from input
    let fileSystem = buildFileSystem inputLines
    -- Calculate sizes of all directories
    let sizeMap = calculateAllSizes fileSystem

    -- Calculate and print Part 1 answer
    let result1 = part1 sizeMap
    putStrLn $ "Part 1: " ++ show result1

    -- Calculate and print Part 2 answer
    let result2 = part2 sizeMap
    putStrLn $ "Part 2: " ++ show result2

    -- Close the file handle
    hClose handle

