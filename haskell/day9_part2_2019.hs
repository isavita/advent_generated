
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

data VMState = VMState {
    memory       :: Map.Map Integer Integer,
    ip           :: Integer,
    relativeBase :: Integer,
    inputVal     :: Integer,
    lastOutput   :: Integer
}

type IntcodeM = State VMState

memRead :: Integer -> IntcodeM Integer
memRead addr = gets (Map.findWithDefault 0 addr . memory)

memWrite :: Integer -> Integer -> IntcodeM ()
memWrite addr val = modify' $ \s -> s { memory = Map.insert addr val (memory s) }

getParam :: Int -> IntcodeM Integer
getParam offset = do
    s <- get
    instruction <- memRead (ip s)
    let modes = instruction `div` 100
    let mode = (modes `div` (10 ^ (offset - 1))) `mod` 10
    paramAddr <- memRead (ip s + fromIntegral offset)
    case mode of
        0 -> memRead paramAddr
        1 -> return paramAddr
        2 -> memRead (relativeBase s + paramAddr)
        _ -> error $ "Invalid parameter mode " ++ show mode

setParam :: Int -> Integer -> IntcodeM ()
setParam offset value = do
    s <- get
    instruction <- memRead (ip s)
    let modes = instruction `div` 100
    outputParamAddr <- memRead (ip s + fromIntegral offset)
    let outputMode = (modes `div` (10 ^ (offset - 1))) `mod` 10
    let finalAddr = case outputMode of
                        0 -> outputParamAddr
                        2 -> relativeBase s + outputParamAddr
                        _ -> error $ "Invalid output parameter mode " ++ show outputMode
    memWrite finalAddr value

runIntcode :: IntcodeM Integer
runIntcode = do
    s <- get
    instruction <- memRead (ip s)
    let opcode = instruction `mod` 100
    case opcode of
        1 -> do p1 <- getParam 1; p2 <- getParam 2; setParam 3 (p1 + p2); modify' $ \st -> st { ip = ip st + 4 }; runIntcode
        2 -> do p1 <- getParam 1; p2 <- getParam 2; setParam 3 (p1 * p2); modify' $ \st -> st { ip = ip st + 4 }; runIntcode
        3 -> do inp <- gets inputVal; setParam 1 inp; modify' $ \st -> st { ip = ip st + 2 }; runIntcode
        4 -> do p1 <- getParam 1; modify' $ \st -> st { lastOutput = p1, ip = ip st + 2 }; runIntcode
        5 -> do p1 <- getParam 1; p2 <- getParam 2; modify' $ \st -> st { ip = if p1 /= 0 then p2 else ip st + 3 }; runIntcode
        6 -> do p1 <- getParam 1; p2 <- getParam 2; modify' $ \st -> st { ip = if p1 == 0 then p2 else ip st + 3 }; runIntcode
        7 -> do p1 <- getParam 1; p2 <- getParam 2; setParam 3 (if p1 < p2 then 1 else 0); modify' $ \st -> st { ip = ip st + 4 }; runIntcode
        8 -> do p1 <- getParam 1; p2 <- getParam 2; setParam 3 (if p1 == p2 then 1 else 0); modify' $ \st -> st { ip = ip st + 4 }; runIntcode
        9 -> do p1 <- getParam 1; modify' $ \st -> st { relativeBase = relativeBase st + p1, ip = ip st + 2 }; runIntcode
        99 -> gets lastOutput
        _ -> error $ "Unknown opcode: " ++ show opcode

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let programList = map read . words $ map (\c -> if c == ',' then ' ' else c) contents :: [Integer]
        initialMemory = Map.fromList $ zip [0..] programList
        initialState = VMState { memory = initialMemory, ip = 0, relativeBase = 0, inputVal = 2, lastOutput = 0 }
        finalOutput = evalState runIntcode initialState
    print finalOutput
