module Main where

import Data.Array.IO
import Control.Monad
import Data.List (isPrefixOf)

type Grid = IOArray (Int, Int) Bool
type Coord = (Int, Int)
type Instruction = (String, Coord, Coord)

-- Create an empty 1000x1000 grid
createGrid :: IO Grid
createGrid = newArray ((0,0), (999,999)) False

-- Parse a coordinate string like "123,456" into a tuple
parseCoord :: String -> Coord
parseCoord s = case break (==',') s of
    (x, ',':y) -> (read x, read y)
    _ -> error "Invalid coordinate format"

-- Parse a single instruction line
parseInstruction :: String -> Instruction
parseInstruction line
    | "turn on " `isPrefixOf` line = parse "turn on" (drop 8 line)
    | "turn off " `isPrefixOf` line = parse "turn off" (drop 9 line)
    | "toggle " `isPrefixOf` line = parse "toggle" (drop 7 line)
    | otherwise = error $ "Invalid instruction: " ++ line
    where
        parse cmd rest = case words rest of
            [start, "through", end] -> (cmd, parseCoord start, parseCoord end)
            _ -> error $ "Invalid instruction format: " ++ line

-- Apply a single instruction to the grid
applyInstruction :: Grid -> Instruction -> IO ()
applyInstruction grid (cmd, (x1,y1), (x2,y2)) = do
    forM_ [x1..x2] $ \x ->
        forM_ [y1..y2] $ \y -> do
            case cmd of
                "turn on" -> writeArray grid (x,y) True
                "turn off" -> writeArray grid (x,y) False
                "toggle" -> do
                    current <- readArray grid (x,y)
                    writeArray grid (x,y) (not current)
                _ -> error $ "Unknown command: " ++ cmd

-- Count how many lights are on
countLights :: Grid -> IO Int
countLights grid = do
    let bounds = ((0,0), (999,999))
    count <- foldM (\acc idx -> do
        val <- readArray grid idx
        return $ acc + if val then 1 else 0) 0 (range bounds)
    return count

main :: IO ()
main = do
    -- Read and parse input
    content <- readFile "input.txt"
    let instructions = map parseInstruction $ lines content
    
    -- Create and initialize grid
    grid <- createGrid
    
    -- Apply all instructions
    mapM_ (applyInstruction grid) instructions
    
    -- Count and display result
    result <- countLights grid
    putStrLn $ "Number of lights lit: " ++ show result
