{-# LANGUAGE BangPatterns, UnboxedTuples #-}
module Main where

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Data.List (foldl')
import qualified Data.ByteString.Char8 as B
import Text.Read (readMaybe)

-- Strict coordinate type
data Coord = Coord {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Show, Eq)

data Command = TurnOn | TurnOff | Toggle
    deriving (Show, Eq)

-- Strict instruction type
data Instruction = Instruction !Command !Coord !Coord
    deriving (Show)

-- Safe parse for a single number from ByteString
parseInt :: B.ByteString -> Int
parseInt bs = case readMaybe (B.unpack bs) of
    Just n -> n
    Nothing -> error $ "Invalid number: " ++ B.unpack bs

-- Parse coordinates more efficiently
parseCoord :: B.ByteString -> Coord
parseCoord s = 
    let (x, rest) = B.break (==',') s
        y = B.tail rest
    in Coord (parseInt x) (parseInt y)

-- Parse a single instruction line more efficiently
parseInstruction :: B.ByteString -> Instruction
parseInstruction line
    | B.pack "turn on " `B.isPrefixOf` line = parse TurnOn (B.drop 8 line)
    | B.pack "turn off " `B.isPrefixOf` line = parse TurnOff (B.drop 9 line)
    | B.pack "toggle " `B.isPrefixOf` line = parse Toggle (B.drop 7 line)
    | otherwise = error $ "Invalid instruction: " ++ show line
    where
        parse cmd rest = 
            let parts = B.words rest
                start = parseCoord (parts !! 0)
                end = parseCoord (parts !! 2)
            in Instruction cmd start end

-- Apply brightness change based on command
getBrightnessChange :: Command -> Int
getBrightnessChange TurnOn = 1
getBrightnessChange TurnOff = -1
getBrightnessChange Toggle = 2

-- Process all instructions at once using array updates
processInstructions :: [Instruction] -> UArray (Int, Int) Int
processInstructions instructions = runSTUArray $ do
    -- Create mutable array
    arr <- newArray ((0,0), (999,999)) 0
    
    -- Process each instruction
    forM_ instructions $ \(Instruction cmd (Coord x1 y1) (Coord x2 y2)) -> do
        let change = getBrightnessChange cmd
        forM_ [x1..x2] $ \x ->
            forM_ [y1..y2] $ \y -> do
                !current <- readArray arr (x,y)
                writeArray arr (x,y) (max 0 (current + change))
    
    return arr

main :: IO ()
main = do
    -- Read input file as strict ByteString
    !content <- B.readFile "input.txt"
    
    -- Parse instructions
    let instructions = map parseInstruction $ B.lines content
    
    -- Process instructions and calculate total brightness
    let finalGrid = processInstructions instructions
        !totalBrightness = foldl' (+) 0 $ elems finalGrid
    
    -- Output result
    putStrLn $ "Total brightness: " ++ show totalBrightness
