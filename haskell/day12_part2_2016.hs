import System.IO

data Instruction = Cpy String String | Inc String | Dec String | Jnz String String deriving (Show)
type Registers = (Int, Int, Int, Int)

main = do
    contents <- readFile "input.txt"
    let instructions = map parseInstruction (lines contents)
    let registers = execute instructions (0, 0, 1, 0) 0
    print $ getRegister registers "a"

parseInstruction :: String -> Instruction
parseInstruction line =
    let parts = words line
    in case parts of
        ["cpy", x, y] -> Cpy x y
        ["inc", x] -> Inc x
        ["dec", x] -> Dec x
        ["jnz", x, y] -> Jnz x y

execute :: [Instruction] -> Registers -> Int -> Registers
execute instructions registers pc
    | pc < 0 || pc >= length instructions = registers
    | otherwise =
        case instructions !! pc of
            Cpy x y -> execute instructions (setRegister registers y (getValue registers x)) (pc + 1)
            Inc x -> execute instructions (setRegister registers x (getRegister registers x + 1)) (pc + 1)
            Dec x -> execute instructions (setRegister registers x (getRegister registers x - 1)) (pc + 1)
            Jnz x y ->
                if getValue registers x /= 0
                then execute instructions registers (pc + read y)
                else execute instructions registers (pc + 1)

getValue :: Registers -> String -> Int
getValue registers x
    | x == "a" = getRegister registers "a"
    | x == "b" = getRegister registers "b"
    | x == "c" = getRegister registers "c"
    | x == "d" = getRegister registers "d"
    | otherwise = read x

getRegister :: Registers -> String -> Int
getRegister (a, b, c, d) x
    | x == "a" = a
    | x == "b" = b
    | x == "c" = c
    | x == "d" = d

setRegister :: Registers -> String -> Int -> Registers
setRegister (a, b, c, d) x value
    | x == "a" = (value, b, c, d)
    | x == "b" = (a, value, c, d)
    | x == "c" = (a, b, value, d)
    | x == "d" = (a, b, c, value)