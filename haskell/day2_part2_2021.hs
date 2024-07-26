
import System.IO
import Control.Monad (forM_)

-- Define a data type to hold the submarine's state
data Submarine = Submarine { horizontal :: Int, depth :: Int, aim :: Int } deriving Show

-- Function to process a command and update the submarine's state
processCommand :: Submarine -> String -> Submarine
processCommand (Submarine h d a) command =
    let (cmd:valStr:_) = words command
        value = read valStr :: Int
    in case cmd of
        "forward" -> Submarine (h + value) (d + a * value) a
        "down"    -> Submarine h d (a + value)
        "up"      -> Submarine h d (a - value)
        _         -> error "Unknown command"

-- Main function to read the input file and calculate the result
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let commands = lines contents
        finalSubmarine = foldl processCommand (Submarine 0 0 0) commands
        result = horizontal finalSubmarine * depth finalSubmarine
    print result
