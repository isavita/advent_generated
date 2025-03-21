
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.IO

type Memory = M.Map Int Int

getParam :: Memory -> Int -> String -> Int -> Int -> Int
getParam memory ip modes offset relativeBase =
  let mode = getMode modes offset
      param = memory M.! (ip + offset)
  in case mode of
       0 -> fromMaybe 0 (M.lookup param memory)
       1 -> param
       2 -> fromMaybe 0 (M.lookup (relativeBase + param) memory)
       _ -> error "unknown parameter mode"

setParam :: Memory -> Int -> String -> Int -> Int -> Int -> Memory
setParam memory ip modes offset relativeBase value =
  let mode = getMode modes offset
      param = memory M.! (ip + offset)
  in case mode of
       0 -> M.insert param value memory
       2 -> M.insert (relativeBase + param) value memory
       _ -> error "unknown parameter mode"

getMode :: String -> Int -> Int
getMode modes offset =
  if length modes >= offset
  then read [modes !! (length modes - offset)]
  else 0

runIntcode :: Memory -> Int -> Int -> Int -> Int
runIntcode memory ip relativeBase output =
  let opcode = memory M.! ip `mod` 100
      modes = show (memory M.! ip `div` 100)
  in case opcode of
       1 -> let newMemory = setParam memory ip modes 3 relativeBase (getParam memory ip modes 1 relativeBase + getParam memory ip modes 2 relativeBase)
            in runIntcode newMemory (ip + 4) relativeBase output
       2 -> let newMemory = setParam memory ip modes 3 relativeBase (getParam memory ip modes 1 relativeBase * getParam memory ip modes 2 relativeBase)
            in runIntcode newMemory (ip + 4) relativeBase output
       3 -> let newMemory = setParam memory ip modes 1 relativeBase 1  -- Test mode input
            in runIntcode newMemory (ip + 2) relativeBase output
       4 -> let newOutput = getParam memory ip modes 1 relativeBase
            in runIntcode memory (ip + 2) relativeBase newOutput
       5 -> if getParam memory ip modes 1 relativeBase /= 0
            then runIntcode memory (getParam memory ip modes 2 relativeBase) relativeBase output
            else runIntcode memory (ip + 3) relativeBase output
       6 -> if getParam memory ip modes 1 relativeBase == 0
            then runIntcode memory (getParam memory ip modes 2 relativeBase) relativeBase output
            else runIntcode memory (ip + 3) relativeBase output
       7 -> let value = if getParam memory ip modes 1 relativeBase < getParam memory ip modes 2 relativeBase then 1 else 0
                newMemory = setParam memory ip modes 3 relativeBase value
            in runIntcode newMemory (ip + 4) relativeBase output
       8 -> let value = if getParam memory ip modes 1 relativeBase == getParam memory ip modes 2 relativeBase then 1 else 0
                newMemory = setParam memory ip modes 3 relativeBase value
            in runIntcode newMemory (ip + 4) relativeBase output
       9 -> let newRelativeBase = relativeBase + getParam memory ip modes 1 relativeBase
            in runIntcode memory (ip + 2) newRelativeBase output
       99 -> output
       _ -> error $ "unknown opcode: " ++ show opcode

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetLine handle
  let program = map read (wordsWhen (== ',') contents) :: [Int]
      memory = M.fromList (zip [0..] program)
      output = runIntcode memory 0 0 0
  print output
  hClose handle

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
