
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow                  ( (>>>) )
import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.List                      ( intercalate )
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe                     ( fromMaybe )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

-- --- Intcode Computer ---
-- (Assuming a standard Intcode implementation exists from previous days)
-- Key components:
-- - State representation (memory, instruction pointer, relative base)
-- - Input/Output handling
-- - Opcode execution

data IntcodeState = IntcodeState
    { memory      :: IM.IntMap Int
    , instructionPointer :: Int
    , relativeBase :: Int
    , inputs      :: [Int]
    , outputs     :: [Int]
    , halted      :: Bool
    } deriving (Show)

-- Initialize Intcode state from program code
initIntcode :: [Int] -> [Int] -> IntcodeState
initIntcode code ins = IntcodeState
    { memory      = IM.fromList $ zip [0 ..] code
    , instructionPointer = 0
    , relativeBase = 0
    , inputs      = ins
    , outputs     = []
    , halted      = False
    }

-- Memory access function
memRead :: IntcodeState -> Int -> Int
memRead state addr = fromMaybe 0 $ IM.lookup addr (memory state)

memWrite :: IntcodeState -> Int -> Int -> IntcodeState
memWrite state addr val = state { memory = IM.insert addr val (memory state) }

-- Parameter mode handling
getParam :: IntcodeState -> Int -> Int
getParam state offset =
    let ip        = instructionPointer state
        modeDigit = (memRead state ip `div` (10 ^ (offset + 1))) `mod` 10
        paramVal  = memRead state (ip + offset)
        rb        = relativeBase state
    in  case modeDigit of
            0 -> memRead state paramVal             -- Position mode
            1 -> paramVal                           -- Immediate mode
            2 -> memRead state (rb + paramVal)     -- Relative mode
            _ -> error $ "Invalid parameter mode: " ++ show modeDigit

-- Get address for writing based on mode
getWriteAddr :: IntcodeState -> Int -> Int
getWriteAddr state offset =
    let ip        = instructionPointer state
        modeDigit = (memRead state ip `div` (10 ^ (offset + 1))) `mod` 10
        paramVal  = memRead state (ip + offset)
        rb        = relativeBase state
    in  case modeDigit of
            0 -> paramVal             -- Position mode
            2 -> rb + paramVal        -- Relative mode
            _ -> error $ "Invalid write parameter mode: " ++ show modeDigit

-- Execute a single Intcode step
stepIntcode :: IntcodeState -> IntcodeState
stepIntcode state
    | halted state = state
    | otherwise =
        let ip      = instructionPointer state
            opcode  = memRead state ip `mod` 100
            param1  = getParam state 1
            param2  = getParam state 2
            addr3   = getWriteAddr state 3
            addr1   = getWriteAddr state 1
            rb      = relativeBase state
            inAddr  = getWriteAddr state 1 -- Input/Output addr calculation is like write addr
        in case opcode of
            1 -> -- Add
                let newState = memWrite state addr3 (param1 + param2)
                in newState { instructionPointer = ip + 4 }
            2 -> -- Multiply
                let newState = memWrite state addr3 (param1 * param2)
                in newState { instructionPointer = ip + 4 }
            3 -> -- Input
                case inputs state of
                    (i:is) -> let newState = memWrite state inAddr i
                              in newState { instructionPointer = ip + 2, inputs = is }
                    []     -> error "Intcode Error: Input requested but none available."
            4 -> -- Output
                let newState = state { outputs = outputs state ++ [param1] } -- Append output
                in newState { instructionPointer = ip + 2 }
            5 -> -- Jump-if-true
                if param1 /= 0
                    then state { instructionPointer = param2 }
                    else state { instructionPointer = ip + 3 }
            6 -> -- Jump-if-false
                if param1 == 0
                    then state { instructionPointer = param2 }
                    else state { instructionPointer = ip + 3 }
            7 -> -- Less than
                let val      = if param1 < param2 then 1 else 0
                    newState = memWrite state addr3 val
                in newState { instructionPointer = ip + 4 }
            8 -> -- Equals
                let val      = if param1 == param2 then 1 else 0
                    newState = memWrite state addr3 val
                in newState { instructionPointer = ip + 4 }
            9 -> -- Adjust relative base
                state { relativeBase       = rb + param1
                      , instructionPointer = ip + 2
                      }
            99 -> -- Halt
                state { halted = True }
            _ -> error $ "Unknown opcode: " ++ show opcode ++ " at IP " ++ show ip

-- Run Intcode until halt
runIntcode :: IntcodeState -> IntcodeState
runIntcode state = head $ dropWhile (not . halted) $ iterate stepIntcode state

-- Helper to convert String to ASCII Int list
stringToAscii :: String -> [Int]
stringToAscii = map ord

-- Helper to parse the input file content
parseInput :: String -> [Int]
parseInput = map read . filter (not . null) . splitOn ','
  where
    splitOn :: Eq a => a -> [a] -> [[a]]
    splitOn delimiter list = case break (== delimiter) list of
        (a, [])      -> [a]
        (a, _ : b) -> a : splitOn delimiter b

-- --- Day 21 Specific Logic ---

-- The springscript program.
-- Logic: Jump if there is a hole within the next 3 tiles (A, B, or C is false)
--        AND the landing tile (D) is ground.
-- J = (!A || !B || !C) && D
springScript :: [String]
springScript =
    [ "NOT A J" -- J = !A
    , "NOT B T" -- T = !B
    , "OR T J"  -- J = J || T = !A || !B
    , "NOT C T" -- T = !C
    , "OR T J"  -- J = J || T = (!A || !B) || !C
    , "AND D J" -- J = J && D = (!A || !B || !C) && D
    , "WALK"    -- Execute the walk sequence
    ]

-- Convert the springscript into Intcode input (ASCII codes)
prepareInput :: [String] -> [Int]
prepareInput scriptLines = stringToAscii $ intercalate "\n" scriptLines ++ "\n"

main :: IO ()
main = do
    -- Read Intcode program from input.txt
    programCodeStr <- readFile "input.txt"
    let programCode = parseInput programCodeStr

    -- Prepare springscript input for Intcode
    let intcodeInput = prepareInput springScript

    -- Initialize and run the Intcode computer
    let initialState = initIntcode programCode intcodeInput
    let finalState   = runIntcode initialState

    -- Process the output
    let finalOutputs = outputs finalState
    case finalOutputs of
        [] -> hPutStrLn stderr "Error: Intcode produced no output."
        -- If the last output is a large number, it's the hull damage
        os | last os > 127 -> print (last os)
           -- Otherwise, print the ASCII output (likely an error message or map)
           | otherwise     -> do
               hPutStrLn stderr "Springdroid failed. ASCII output:"
               putStrLn $ map chr os

