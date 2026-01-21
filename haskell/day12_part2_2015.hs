
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative

data J = N Int | S String | A [J] | O [J] deriving Eq

jP :: ReadP J
jP = skipSpaces *> (nP <|> sP <|> aP <|> oP)
  where
    nP = N . read <$> munch1 (\c -> isDigit c || c == '-')
    sP = S <$> (char '"' *> munch (/= '"') <* char '"')
    aP = A <$> between (char '[') (skipSpaces *> char ']') (sepBy jP (char ','))
    oP = O <$> between (char '{') (skipSpaces *> char '}') (sepBy pair (char ','))
    pair = jP *> skipSpaces *> char ':' *> jP

solve :: J -> Int
solve (N n) = n
solve (A l) = sum $ map solve l
solve (O l) = if S "red" `elem` l then 0 else sum $ map solve l
solve _     = 0

main :: IO ()
main = do
    content <- readFile "input.txt"
    let parses = readP_to_S (jP <* skipSpaces <* eof) content
    case parses of
        ((result, _):_) -> print $ solve result
        _               -> return ()

