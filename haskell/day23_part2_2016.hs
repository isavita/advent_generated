
import System.IO
import Data.Array.IO
import Control.Monad
import Data.Char (isDigit)

type Instr = [String]

isReg :: String -> Bool
isReg [c] = c `elem` "abcd"
isReg _   = False

regIdx :: String -> Int
regIdx "a" = 0
regIdx "b" = 1
regIdx "c" = 2
regIdx "d" = 3
regIdx _   = error "bad register"

getVal :: String -> IOUArray Int Int -> IO Int
getVal s regs
  | isReg s   = readArray regs (regIdx s)
  | otherwise = return (read s)

modifyReg :: IOUArray Int Int -> String -> (Int -> Int) -> IO ()
modifyReg regs r f = readArray regs i >>= writeArray regs i . f where i = regIdx r

main :: IO ()
main = do
  content <- readFile "input.txt"
  let instrList = map words (lines content)
  instrArr <- newListArray (0, length instrList - 1) instrList :: IO (IOArray Int Instr)
  regs <- newArray (0,3) 0 :: IO (IOUArray Int Int)
  writeArray regs 0 12
  let n = length instrList
  let loop i = when (i < n) $ do
        opt <- if i + 5 < n
          then do p0 <- readArray instrArr i
                  p1 <- readArray instrArr (i+1)
                  p2 <- readArray instrArr (i+2)
                  p3 <- readArray instrArr (i+3)
                  p4 <- readArray instrArr (i+4)
                  p5 <- readArray instrArr (i+5)
                  if head p0=="cpy" && head p1=="inc" && head p2=="dec"
                     && head p3=="jnz" && head p4=="dec" && head p5=="jnz"
                    then let cpyX = p0!!1; cpyY = p0!!2
                             incA = p1!!1; decC = p2!!1
                             jnzC = p3!!1; offC = p3!!2
                             decD = p4!!1; jnzD = p5!!1; offD = p5!!2
                         in if incA=="a" && decC==cpyY && jnzC==cpyY && offC=="-2"
                               && decD=="d" && jnzD=="d" && offD=="-5"
                            then do valX <- getVal cpyX regs
                                    valD <- readArray regs 3
                                    aVal <- readArray regs 0
                                    writeArray regs 0 (aVal + valX * valD)
                                    writeArray regs (regIdx cpyY) 0
                                    writeArray regs 3 0
                                    return True
                            else return False
                    else return False
          else return False
        if opt then loop (i+6) else do
          instr <- readArray instrArr i
          case instr of
            ("tgl":x:_) -> do
              v <- getVal x regs
              let t = i + v
              when (t >= 0 && t < n) $ do
                tInstr <- readArray instrArr t
                let newInstr = case tInstr of
                      [_op,_arg] -> if head tInstr == "inc"
                                    then ["dec",tInstr!!1] else ["inc",tInstr!!1]
                      [_op,_arg1,_arg2] -> if head tInstr == "jnz"
                                           then ["cpy",tInstr!!1,tInstr!!2]
                                           else ["jnz",tInstr!!1,tInstr!!2]
                      _ -> tInstr
                writeArray instrArr t newInstr
              loop (i+1)
            ("cpy":x:y:_) -> do
              when (isReg y) $ getVal x regs >>= writeArray regs (regIdx y)
              loop (i+1)
            ("inc":x:_) -> do
              when (isReg x) $ modifyReg regs x (+1)
              loop (i+1)
            ("dec":x:_) -> do
              when (isReg x) $ modifyReg regs x (subtract 1)
              loop (i+1)
            ("jnz":x:y:_) -> do
              vx <- getVal x regs
              vy <- getVal y regs
              if vx /= 0 then loop (i + vy) else loop (i+1)
            _ -> loop (i+1)
  loop 0
  aRes <- readArray regs 0
  print aRes
