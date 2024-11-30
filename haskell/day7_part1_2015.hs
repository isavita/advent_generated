import Data.Bits
import Data.Word (Word16)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Char (isDigit)
import Control.Monad.State

-- Define Operand as either a constant or a wire reference
data Operand = Const Word16 | Wire String deriving (Show, Eq, Ord)

-- Define Expr to represent different operations
data Expr =
    EAssign Operand
  | EAnd Operand Operand
  | EOr Operand Operand
  | ELShift Operand Int
  | ERShift Operand Int
  | ENot Operand
  deriving (Show, Eq, Ord)

-- Parse a single operand
parseOperand :: String -> Operand
parseOperand s = if all isDigit s
                then Const (fromIntegral (read s :: Int))
                else Wire s

-- Parse a single line into (target wire, expression)
parseLine :: String -> (String, Expr)
parseLine line =
    let tokens = words line
        target = last tokens
        exprTokens = take (length tokens - 2) tokens
    in case exprTokens of
        [x] -> (target, EAssign (parseOperand x))
        ["NOT", x] -> (target, ENot (parseOperand x))
        [x, "AND", y] -> (target, EAnd (parseOperand x) (parseOperand y))
        [x, "OR", y] -> (target, EOr (parseOperand x) (parseOperand y))
        [x, "LSHIFT", n] -> (target, ELShift (parseOperand x) (read n))
        [x, "RSHIFT", n] -> (target, ERShift (parseOperand x) (read n))
        _ -> error ("Unknown instruction: " ++ line)

-- Type aliases for clarity
type WireMap = Map String Expr
type Memo = Map String Word16

-- Evaluate the signal for a given wire using the State monad for memoization
evalWire :: WireMap -> String -> State Memo Word16
evalWire wireMap wire = do
    memo <- get
    case Map.lookup wire memo of
        Just val -> return val
        Nothing -> case Map.lookup wire wireMap of
            Nothing -> 
                if all isDigit wire
                    then let val = fromIntegral (read wire :: Int) in do
                        modify (Map.insert wire val)
                        return val
                    else error ("Wire '" ++ wire ++ "' not found in the instructions.")
            Just expr -> do
                val <- evalExpr wireMap expr
                modify (Map.insert wire val)
                return val

-- Evaluate an expression and return its signal
evalExpr :: WireMap -> Expr -> State Memo Word16
evalExpr wireMap expr = case expr of
    EAssign op -> evalOperand wireMap op
    EAnd op1 op2 -> do
        val1 <- evalOperand wireMap op1
        val2 <- evalOperand wireMap op2
        return (val1 .&. val2)
    EOr op1 op2 -> do
        val1 <- evalOperand wireMap op1
        val2 <- evalOperand wireMap op2
        return (val1 .|. val2)
    ELShift op n -> do
        val <- evalOperand wireMap op
        return (val `shiftL` n)
    ERShift op n -> do
        val <- evalOperand wireMap op
        return (val `shiftR` n)
    ENot op -> do
        val <- evalOperand wireMap op
        return (complement val)

-- Helper function to evaluate an operand
evalOperand :: WireMap -> Operand -> State Memo Word16
evalOperand _ (Const n) = return n
evalOperand wireMap (Wire w) = evalWire wireMap w

-- Main function to read input, process, and output the result
main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesInput = lines content
        parsed = map parseLine linesInput
        wireMap = Map.fromList parsed
        signalA = evalState (evalWire wireMap "a") Map.empty
    putStrLn $ "The signal provided to wire 'a' is: " ++ show signalA
