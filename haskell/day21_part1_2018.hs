import System.IO
import Data.Array
import Data.Bits
import Data.Char

data Op = ADDR|ADDI|MULR|MULI|BANR|BANI|BORR|BORI|SETR|SETI|GTIR|GTRI|GTRR|EQIR|EQRI|EQRR
type Instr = (Op,Int,Int,Int)
type Regs = Array Int Int

parseOp :: String -> Op
parseOp "addr" = ADDR
parseOp "addi" = ADDI
parseOp "mulr" = MULR
parseOp "muli" = MULI
parseOp "banr" = BANR
parseOp "bani" = BANI
parseOp "borr" = BORR
parseOp "bori" = BORI
parseOp "setr" = SETR
parseOp "seti" = SETI
parseOp "gtir" = GTIR
parseOp "gtri" = GTRI
parseOp "gtrr" = GTRR
parseOp "eqir" = EQIR
parseOp "eqri" = EQRI
parseOp "eqrr" = EQRR
parseOp _ = error "unknown op"

toInstr :: [String] -> Instr
toInstr (n:a:b:c:_) = (parseOp n, read a, read b, read c)
toInstr _ = error "bad instr"

apply :: Regs -> Instr -> Regs
apply regs (op,a,b,c) =
  let va i = regs ! i
      res = case op of
        ADDR -> va a + va b
        ADDI -> va a + b
        MULR -> va a * va b
        MULI -> va a * b
        BANR -> va a .&. va b
        BANI -> va a .&. b
        BORR -> va a .|. va b
        BORI -> va a .|. b
        SETR -> va a
        SETI -> a
        GTIR -> if a > va b then 1 else 0
        GTRI -> if va a > b then 1 else 0
        GTRR -> if va a > va b then 1 else 0
        EQIR -> if a == va b then 1 else 0
        EQRI -> if va a == b then 1 else 0
        EQRR -> if va a == va b then 1 else 0
  in regs // [(c,res)]

tick :: Array Int Instr -> Int -> Regs -> (Bool,Regs)
tick instrs ipIdx regs =
  let n = let (l,h) = bounds instrs in h - l + 1
      ip = regs ! ipIdx
  in if ip < 0 || ip >= n then (True, regs)
     else
       let inst = instrs ! ip
           regs' = apply regs inst
           regs'' = regs' // [(ipIdx, regs' ! ipIdx + 1)]
       in if regs'' ! ipIdx < 0 || regs'' ! ipIdx >= n then (True, regs'') else (False, regs'')

run :: Array Int Instr -> Int -> Regs -> Regs
run instrs ipIdx regs =
  case tick instrs ipIdx regs of
    (True, r) -> r
    (False, r) -> if r ! ipIdx == 28 then r else run instrs ipIdx r

main :: IO ()
main = do
  s <- readFile "input.txt"
  let ls = filter (not . all isSpace) (lines s)
      ipIdx = read (words (head ls) !! 1) :: Int
      instrs = map (toInstr . words) (tail ls)
      arr = listArray (0, length instrs - 1) instrs
      regs0 = listArray (0,5) (replicate 6 0)
      final = run arr ipIdx regs0
  print (final ! 5)