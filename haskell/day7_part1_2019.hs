
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Concurrent
import Control.Monad

type Memory = Map.Map Int Int
type Program = [Int]
type Address = Int
type Input = Chan Int
type Output = Chan Int

data VM = VM {
    memory :: Memory,
    ip :: Int,
    input :: Input,
    output :: Output
}

opCode :: Int -> Int
opCode cmd = cmd `mod` 100

immediate :: Int -> Int -> Bool
immediate cmd paramNum = (cmd `div` (10 ^ (paramNum + 1))) `mod` 10 == 1

getParam :: VM -> Int -> Bool -> Int
getParam vm addr immediate =
    let param = fromJust $ Map.lookup addr (memory vm)
    in if immediate
        then param
        else fromJust $ Map.lookup param (memory vm)

loadProgram :: String -> IO Program
loadProgram filename = do
    contents <- readFile filename
    return $ map read $ words $ map (\c -> if c == ',' then ' ' else c) contents

runVM :: VM -> IO ()
runVM vm = do
    let cmd = fromJust $ Map.lookup (ip vm) (memory vm)
    case opCode cmd of
        1 -> do
            let param1 = getParam vm (ip vm + 1) (immediate cmd 1)
            let param2 = getParam vm (ip vm + 2) (immediate cmd 2)
            let address = getParam vm (ip vm + 3) True
            let newMemory = Map.insert address (param1 + param2) (memory vm)
            runVM $ vm { memory = newMemory, ip = ip vm + 4 }
        2 -> do
            let param1 = getParam vm (ip vm + 1) (immediate cmd 1)
            let param2 = getParam vm (ip vm + 2) (immediate cmd 2)
            let address = getParam vm (ip vm + 3) True
            let newMemory = Map.insert address (param1 * param2) (memory vm)
            runVM $ vm { memory = newMemory, ip = ip vm + 4 }
        3 -> do
            address <- readChan (input vm)
            let newMemory = Map.insert (getParam vm (ip vm + 1) True) address (memory vm)
            runVM $ vm { memory = newMemory, ip = ip vm + 2 }
        4 -> do
            let param1 = getParam vm (ip vm + 1) (immediate cmd 1)
            writeChan (output vm) param1
            runVM $ vm { ip = ip vm + 2 }
        5 -> do
            let param1 = getParam vm (ip vm + 1) (immediate cmd 1)
            let param2 = getParam vm (ip vm + 2) (immediate cmd 2)
            if param1 /= 0
                then runVM $ vm { ip = param2 }
                else runVM $ vm { ip = ip vm + 3 }
        6 -> do
            let param1 = getParam vm (ip vm + 1) (immediate cmd 1)
            let param2 = getParam vm (ip vm + 2) (immediate cmd 2)
            if param1 == 0
                then runVM $ vm { ip = param2 }
                else runVM $ vm { ip = ip vm + 3 }
        7 -> do
            let param1 = getParam vm (ip vm + 1) (immediate cmd 1)
            let param2 = getParam vm (ip vm + 2) (immediate cmd 2)
            let address = getParam vm (ip vm + 3) True
            let newMemory = Map.insert address (if param1 < param2 then 1 else 0) (memory vm)
            runVM $ vm { memory = newMemory, ip = ip vm + 4 }
        8 -> do
            let param1 = getParam vm (ip vm + 1) (immediate cmd 1)
            let param2 = getParam vm (ip vm + 2) (immediate cmd 2)
            let address = getParam vm (ip vm + 3) True
            let newMemory = Map.insert address (if param1 == param2 then 1 else 0) (memory vm)
            runVM $ vm { memory = newMemory, ip = ip vm + 4 }
        99 -> return ()
        _ -> error $ "Invalid opcode: " ++ show cmd

newVM :: Program -> Input -> Output -> VM
newVM program input output = VM {
    memory = Map.fromList $ zip [0..] program,
    ip = 0,
    input = input,
    output = output
}

runAmplifier :: Program -> [Int] -> IO Int
runAmplifier program phases = do
    chs <- replicateM 6 newChan
    let vms = zipWith3 newVM (repeat program) chs (tail chs)
    forM_ (zip phases vms) $ \(phase, vm) -> do
        writeChan (input vm) phase
        forkIO $ runVM vm
    writeChan (head chs) 0
    readChan (last chs)

main :: IO ()
main = do
    program <- loadProgram "input.txt"
    let phaseSettings = permutations [0, 1, 2, 3, 4]
    results <- mapM (runAmplifier program) phaseSettings
    print $ maximum results
