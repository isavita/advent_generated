
{-# LANGUAGE TupleSections #-}

import Data.Char (isDigit)
import Control.Applicative ((<|>))

data SnailNumber = Regular Int | Pair SnailNumber SnailNumber deriving (Show, Eq)

parseSnailNumber :: String -> SnailNumber
parseSnailNumber s = fst $ parse "" s
  where
    parse :: String -> String -> (SnailNumber, String)
    parse acc (c:cs)
      | isDigit c = let (digits, rest) = span isDigit (c:cs) in (Regular (read digits), rest)
      | c == '[' =
        let (left, rest1) = parse "" cs
            (right, rest2) = parse "" (tail rest1)
        in (Pair left right, tail rest2)

add :: SnailNumber -> SnailNumber -> SnailNumber
add a b = reduce (Pair a b)

reduce :: SnailNumber -> SnailNumber
reduce x = case explode 0 x of
    (True, n, _, _) -> reduce n
    (False, n, _, _) -> case split n of
        (True, n') -> reduce n'
        (False, n') -> n'

explode :: Int -> SnailNumber -> (Bool, SnailNumber, Int, Int)
explode _ (Regular x) = (False, Regular x, 0, 0)
explode 4 (Pair (Regular l) (Regular r)) = (True, Regular 0, l, r)
explode d (Pair l r) =
  case explode (d + 1) l of
    (True, newL, leftV, rightV) ->
      (True, Pair newL (addLeft r rightV), leftV, 0)
    (False, _, _, _) ->
      case explode (d + 1) r of
        (True, newR, leftV, rightV) ->
          (True, Pair (addRight l leftV) newR, 0, rightV)
        (False, _, _, _) -> (False, Pair l r, 0, 0)

addLeft :: SnailNumber -> Int -> SnailNumber
addLeft (Regular x) v = Regular (x + v)
addLeft (Pair l r) v = Pair (addLeft l v) r

addRight :: SnailNumber -> Int -> SnailNumber
addRight (Regular x) v = Regular (x + v)
addRight (Pair l r) v = Pair l (addRight r v)

split :: SnailNumber -> (Bool, SnailNumber)
split (Regular x)
  | x >= 10 = (True, Pair (Regular (x `div` 2)) (Regular ((x + 1) `div` 2)))
  | otherwise = (False, Regular x)
split (Pair l r) =
  case split l of
    (True, newL) -> (True, Pair newL r)
    (False, _) ->
      case split r of
        (True, newR) -> (True, Pair l newR)
        (False, _) -> (False, Pair l r)

magnitude :: SnailNumber -> Int
magnitude (Regular x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let snailNumbers = map parseSnailNumber (lines contents)
  let magnitudes = [magnitude (add a b) | a <- snailNumbers, b <- snailNumbers, a /= b]
  print $ maximum magnitudes
