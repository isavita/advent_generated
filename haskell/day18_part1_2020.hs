import System.IO

data Token = Number Int | Plus | Multiply | LParen | RParen deriving (Show, Eq)

-- Tokenize the input string
tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('+':xs) = Plus : tokenize xs
tokenize ('*':xs) = Multiply : tokenize xs
tokenize ('(':xs) = LParen : tokenize xs
tokenize (')':xs) = RParen : tokenize xs
tokenize xs = let (num, rest) = span (`elem` ['0'..'9']) xs
              in Number (read num) : tokenize rest

-- Parse the tokens into an expression
parseExpression :: [Token] -> (Int, [Token])
parseExpression tokens = parseTerm tokens

parseTerm :: [Token] -> (Int, [Token])
parseTerm tokens = let (value, rest) = parseFactor tokens
                   in parseTerm' value rest

parseTerm' :: Int -> [Token] -> (Int, [Token])
parseTerm' acc (Plus:tokens) = let (value, rest) = parseFactor tokens
                               in parseTerm' (acc + value) rest
parseTerm' acc (Multiply:tokens) = let (value, rest) = parseFactor tokens
                                   in parseTerm' (acc * value) rest
parseTerm' acc tokens = (acc, tokens)

parseFactor :: [Token] -> (Int, [Token])
parseFactor (Number n:tokens) = (n, tokens)
parseFactor (LParen:tokens) = let (value, rest) = parseExpression tokens
                              in case rest of
                                  (RParen:tokens') -> (value, tokens')
                                  _ -> error "Mismatched parentheses"
parseFactor _ = error "Unexpected token"

-- Evaluate a single line of the expression
evaluateLine :: String -> Int
evaluateLine line = let tokens = tokenize line
                        (result, _) = parseExpression tokens
                    in result

-- Main function to read the file and evaluate the expressions
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let result = sum $ map evaluateLine (lines contents)
    print result