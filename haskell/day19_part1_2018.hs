import System.IO
import Data.Bits

data Instruction = Instruction String Int Int Int

main = do
    contents <- readFile "input.txt"
    let (ipBinding:instructions) = lines contents
        ipRegister = read (drop 4 ipBinding) :: Int
        parsedInstructions = map parseInstruction instructions
        registers = replicate 6 0
        finalRegisters = executeProgram ipRegister parsedInstructions registers 0
    print (finalRegisters !! 0)

parseInstruction :: String -> Instruction
parseInstruction line = let [op, a, b, c] = words line
                        in Instruction op (read a) (read b) (read c)

executeProgram :: Int -> [Instruction] -> [Int] -> Int -> [Int]
executeProgram ipRegister instructions registers ip
    | ip < 0 || ip >= length instructions = registers
    | otherwise = let registers' = updateRegister registers ipRegister ip
                      Instruction op a b c = instructions !! ip
                      newRegisters = executeInstruction op a b c registers'
                      newIp = (newRegisters !! ipRegister) + 1
                  in executeProgram ipRegister instructions newRegisters newIp

updateRegister :: [Int] -> Int -> Int -> [Int]
updateRegister registers ipRegister ip = take ipRegister registers ++ [ip] ++ drop (ipRegister + 1) registers

executeInstruction :: String -> Int -> Int -> Int -> [Int] -> [Int]
executeInstruction "addr" a b c registers = updateRegisterValue registers c ((registers !! a) + (registers !! b))
executeInstruction "addi" a b c registers = updateRegisterValue registers c ((registers !! a) + b)
executeInstruction "mulr" a b c registers = updateRegisterValue registers c ((registers !! a) * (registers !! b))
executeInstruction "muli" a b c registers = updateRegisterValue registers c ((registers !! a) * b)
executeInstruction "banr" a b c registers = updateRegisterValue registers c ((registers !! a) .&. (registers !! b))
executeInstruction "bani" a b c registers = updateRegisterValue registers c ((registers !! a) .&. b)
executeInstruction "borr" a b c registers = updateRegisterValue registers c ((registers !! a) .|. (registers !! b))
executeInstruction "bori" a b c registers = updateRegisterValue registers c ((registers !! a) .|. b)
executeInstruction "setr" a _ c registers = updateRegisterValue registers c (registers !! a)
executeInstruction "seti" a _ c registers = updateRegisterValue registers c a
executeInstruction "gtir" a b c registers = updateRegisterValue registers c (if a > (registers !! b) then 1 else 0)
executeInstruction "gtri" a b c registers = updateRegisterValue registers c (if (registers !! a) > b then 1 else 0)
executeInstruction "gtrr" a b c registers = updateRegisterValue registers c (if (registers !! a) > (registers !! b) then 1 else 0)
executeInstruction "eqir" a b c registers = updateRegisterValue registers c (if a == (registers !! b) then 1 else 0)
executeInstruction "eqri" a b c registers = updateRegisterValue registers c (if (registers !! a) == b then 1 else 0)
executeInstruction "eqrr" a b c registers = updateRegisterValue registers c (if (registers !! a) == (registers !! b) then 1 else 0)

updateRegisterValue :: [Int] -> Int -> Int -> [Int]
updateRegisterValue registers index value = take index registers ++ [value] ++ drop (index + 1) registers