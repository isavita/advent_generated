
import qualified Data.Map.Strict as M
import Data.Bits
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import System.IO (readFile)
import Text.Read (readMaybe)
import Data.Char (isDigit)

-- Data type to represent the instructions
data Instruction
  = SetMask String -- The mask string
  | MemWrite Int Int -- Address and Value
  deriving (Show)

-- Type alias for memory state (Address -> Value)
type Memory = M.Map Int Int

-- Type alias for Part 1 mask representation (andMask, orMask)
type MaskV1 = (Int, Int)

-- Type alias for Part 2 mask representation (orMask, floatingIndices)
type MaskV2 = (Int, [Int])

-- Function to parse a single line into an Instruction
parseLine :: String -> Maybe Instruction
parseLine line
    | take 7 line == "mask = " = Just $ SetMask (drop 7 line)
    | take 4 line == "mem[" =
        let (addrStr, rest1) = span isDigit (drop 4 line)
            valStr = drop (length "] = ") rest1
        in case (readMaybe addrStr, readMaybe valStr) of
             (Just addr, Just val) -> Just $ MemWrite addr val
             _                     -> Nothing -- Invalid mem format
    | otherwise = Nothing -- Skip or error on unknown lines

-- Function to parse the whole input string
parseInput :: String -> [Instruction]
parseInput = mapMaybe parseLine . lines

-- --- Part 1 Logic ---

-- Process mask string into (andMask, orMask) for V1
-- andMask: 0 where mask is '0', 1 otherwise
-- orMask:  1 where mask is '1', 0 otherwise
processMaskV1 :: String -> MaskV1
processMaskV1 = foldl' go (complement 0, 0)
  where
    go :: MaskV1 -> Char -> MaskV1
    go (andMask, orMask) c =
      let andMask' = shiftL andMask 1
          orMask'  = shiftL orMask 1
      in case c of
           '0' -> (andMask', orMask')          -- Clear bit in andMask (implicitly 0), keep 0 in orMask
           '1' -> (andMask' .|. 1, orMask' .|. 1) -- Set bit to 1 in andMask, set bit to 1 in orMask
           'X' -> (andMask' .|. 1, orMask')      -- Set bit to 1 in andMask, keep 0 in orMask
           _   -> error "Invalid mask character" -- Should not happen with valid input

-- Apply V1 mask to a value
applyMaskV1 :: MaskV1 -> Int -> Int
applyMaskV1 (andMask, orMask) value = (value .&. andMask) .|. orMask

-- Process all instructions for Part 1
processInstructionsV1 :: [Instruction] -> Memory
processInstructionsV1 instructions = snd $ foldl' step initState instructions
  where
    -- Initial state: mask doesn't change anything, empty memory
    initState :: (MaskV1, Memory)
    initState = (processMaskV1 (replicate 36 'X'), M.empty)

    step :: (MaskV1, Memory) -> Instruction -> (MaskV1, Memory)
    step (_, memory) (SetMask maskStr) =
        (processMaskV1 maskStr, memory)
    step (currentMask, memory) (MemWrite addr value) =
        let maskedValue = applyMaskV1 currentMask value
        in (currentMask, M.insert addr maskedValue memory)

-- Calculate the sum of values in memory for Part 1
solvePart1 :: [Instruction] -> Integer
solvePart1 = sum . map toInteger . M.elems . processInstructionsV1

-- --- Part 2 Logic ---

-- Process mask string into (orMask, floatingIndices) for V2
-- orMask:          1 where mask is '1', 0 otherwise
-- floatingIndices: List of bit positions (0-35) where mask is 'X'
processMaskV2 :: String -> MaskV2
processMaskV2 maskStr = foldl' go (0, []) (zip maskStr [35, 34 .. 0])
  where
    go :: MaskV2 -> (Char, Int) -> MaskV2
    go (orMask, floating) (maskChar, bitIndex) =
        case maskChar of
            '1' -> (setBit orMask bitIndex, floating)
            'X' -> (orMask, bitIndex : floating) -- Collect indices of floating bits
            '0' -> (orMask, floating)
            _   -> error "Invalid mask character"

-- Generate all possible addresses based on V2 mask rules
generateAddressesV2 :: MaskV2 -> Int -> [Int]
generateAddressesV2 (orMask, floatingIndices) initialAddr =
    -- 1. Apply the '1's from the mask to the initial address
    let addrWithOnes = initialAddr .|. orMask
    -- 2. Start with this address and generate combinations for floating bits
    in foldl' expandFloating [addrWithOnes] floatingIndices
  where
    -- For each floating bit index, duplicate the current address list,
    -- setting the bit to 0 in one copy and 1 in the other.
    expandFloating :: [Int] -> Int -> [Int]
    expandFloating currentAddrs floatIndex =
        concatMap (\a -> [clearBit a floatIndex, setBit a floatIndex]) currentAddrs

-- Process all instructions for Part 2
processInstructionsV2 :: [Instruction] -> Memory
processInstructionsV2 instructions = snd $ foldl' step initState instructions
  where
    -- Initial state: placeholder mask, empty memory
    initState :: (MaskV2, Memory)
    initState = ((0, []), M.empty)

    step :: (MaskV2, Memory) -> Instruction -> (MaskV2, Memory)
    step (_, memory) (SetMask maskStr) =
        (processMaskV2 maskStr, memory)
    step (currentMask, memory) (MemWrite addr value) =
        let targetAddresses = generateAddressesV2 currentMask addr
            -- Apply the write operation (value) to all generated addresses
            newMemory = foldl' (\mem a -> M.insert a value mem) memory targetAddresses
        in (currentMask, newMemory)

-- Calculate the sum of values in memory for Part 2
solvePart2 :: [Instruction] -> Integer
solvePart2 = sum . map toInteger . M.elems . processInstructionsV2

-- --- Main Execution ---

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = parseInput input

    -- Solve and Print Part 1
    let result1 = solvePart1 instructions
    putStrLn $ "Part 1: " ++ show result1

    -- Solve and Print Part 2
    let result2 = solvePart2 instructions
    putStrLn $ "Part 2: " ++ show result2

