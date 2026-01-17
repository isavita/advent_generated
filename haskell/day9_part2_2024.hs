
{-# LANGUAGE BangPatterns #-}

import Data.Char (digitToInt)
import Data.List (foldl')
import System.IO

-- File structure: ID, starting position, and size
data File = File { fid :: !Int, fpos :: !Int, fsize :: !Int }

-- Gap structure: starting position and size
data Gap = Gap { gpos :: !Int, gsize :: !Int }

-- Parse the input disk map into a list of Files and a list of Gaps
parseInput :: String -> ([File], [Gap])
parseInput s = go 0 0 True (filter (\c -> c >= '0' && c <= '9') s)
  where
    go _ _ _ [] = ([], [])
    go i p isFile (c:cs)
        | isFile = 
            let (fs, gs) = go (i + 1) (p + d) False cs
            in (File i p d : fs, gs)
        | d > 0 = 
            let (fs, gs) = go i (p + d) True cs
            in (fs, Gap p d : gs)
        | otherwise = go i (p + d) True cs
      where d = digitToInt c

-- Find the leftmost available gap that can fit the file and return the updated gap list
findGap :: File -> [Gap] -> Maybe (Int, [Gap])
findGap _ [] = Nothing
findGap f (g:gs)
    | gpos g >= fpos f = Nothing 
    | gsize g >= fsize f = 
        let updatedGaps = if gsize g == fsize f 
                          then gs 
                          else Gap (gpos g + fsize f) (gsize g - fsize f) : gs
        in Just (gpos g, updatedGaps)
    | otherwise = case findGap f gs of
        Just (pos, gs') -> Just (pos, g : gs')
        Nothing -> Nothing

-- Move each file starting from the highest file ID to the lowest
moveFiles :: [File] -> [Gap] -> [File]
moveFiles files initialGaps = 
    -- Process files in descending order of ID
    let sortedFiles = reverse files 
        (finalFiles, _) = foldl' moveOne ([], initialGaps) sortedFiles
    in finalFiles
  where
    moveOne (acc, currentGaps) f =
        case findGap f currentGaps of
            Nothing -> (f : acc, currentGaps)
            Just (newPos, updatedGaps) -> 
                (f { fpos = newPos } : acc, updatedGaps)

-- Calculate the checksum for a given file using the arithmetic series sum formula
calculateChecksum :: File -> Int
calculateChecksum (File id pos size) =
    -- Sum of (pos + i) for i from 0 to size-1 is: size * (2 * pos + size - 1) / 2
    let rangeSum = (size * (2 * pos + size - 1)) `div` 2
    in id * rangeSum

main :: IO ()
main = do
    -- Read input from input.txt
    content <- readFile "input.txt"
    -- The disk map is a single long line
    let diskMap = head (lines content)
    
    -- Parse initial state
    let (files, gaps) = parseInput diskMap
    
    -- Compact the drive by moving whole files
    let compactedFiles = moveFiles files gaps
    
    -- Compute the resulting filesystem checksum
    let result = sum $ map calculateChecksum compactedFiles
    
    -- Print the output to standard output
    print result
