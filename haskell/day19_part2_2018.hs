
import Data.Bits
import Data.List
import Data.Maybe
import System.IO
import Text.Read

type Reg = [Int]

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving (Show, Eq, Enum)

data Instr = Instr { op :: Op, a :: Int, b :: Int, c :: Int } deriving Show

parseOpcode :: String -> Maybe Op
parseOpcode s = case s of
    "addr" -> Just Addr
    "addi" -> Just Addi
    "mulr" -> Just Mulr
    "muli" -> Just Muli
    "banr" -> Just Banr
    "bani" -> Just Bani
    "borr" -> Just Borr
    "bori" -> Just Bori
    "setr" -> Just Setr
    "seti" -> Just Seti
    "gtir" -> Just Gtir
    "gtri" -> Just Gtri
    "gtrr" -> Just Gtrr
    "eqir" -> Just Eqir
    "eqri" -> Just Eqri
    "eqrr" -> Just Eqrr
    _ -> Nothing

runOp :: Op -> Reg -> Int -> Int -> Int
runOp op r a b = case op of
    Addr -> r!!a + r!!b
    Addi -> r!!a + b
    Mulr -> r!!a * r!!b
    Muli -> r!!a * b
    Banr -> r!!a .&. r!!b
    Bani -> r!!a .&. b
    Borr -> r!!a .|. r!!b
    Bori -> r!!a .|. b
    Setr -> r!!a
    Seti -> a
    Gtir -> if a > r!!b then 1 else 0
    Gtri -> if r!!a > b then 1 else 0
    Gtrr -> if r!!a > r!!b then 1 else 0
    Eqir -> if a == r!!b then 1 else 0
    Eqri -> if r!!a == b then 1 else 0
    Eqrr -> if r!!a == r!!b then 1 else 0

runProgram :: Int -> [Instr] -> Reg -> Int -> Reg
runProgram ipReg program regs maxCycles = go regs 0 0
  where
    go r ip cycles
        | ip < 0 || ip >= length program || cycles >= maxCycles = r
        | otherwise =
            let r' = take ipReg r ++ [ip] ++ drop (ipReg + 1) r
                Instr op a b c = program !! ip
                val = runOp op r' a b
                r'' = take c r' ++ [val] ++ drop (c + 1) r'
                ip' = r'' !! ipReg + 1
            in go r'' ip' (cycles + 1)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
        ipReg = read (words (head ls) !! 1) :: Int
        program = map (parseInstr . words) (drop 1 ls)
        parseInstr [opStr, a, b, c] = Instr (fromJust $ parseOpcode opStr) (read a) (read b) (read c)
        parseInstr _ = error "Invalid instruction"
        regs = runProgram ipReg program [1,0,0,0,0,0] 1000
        n = maximum regs
        total = sum [i | i <- [1..n], n `mod` i == 0]
    print total
