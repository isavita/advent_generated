
{-# OPTIONS_GHC -O2 #-}

import System.IO (readFile)
import Data.List (foldl', isPrefixOf)

-- Constants matching the Python code
size :: Integer
size = 119315717514047

iter :: Integer
iter = 101741582076661

targetPos :: Integer
targetPos = 2020

-- Modular exponentiation: (base^exp) `mod` m using exponentiation by squaring
powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod base exp m
    | even exp = let half = powMod base (exp `div` 2) m
                 in (half * half) `mod` m
    | otherwise = (base * powMod base (exp - 1) m) `mod` m

-- Modular inverse using Fermat's Little Theorem: a^(m-2) `mod` m (m must be prime)
-- Assumes size is prime, which it is.
modInverse :: Integer -> Integer -> Integer
modInverse n m = powMod n (m - 2) m

-- Apply shuffle transformation based on input line.
-- This function calculates the coefficients (a, b) of the linear transformation
-- representing the shuffle process, interpreted as: card_index = (a * position + b) mod size.
-- This matches the derivation based on the Python code's second loop and final calculation.
applyShuffle :: (Integer, Integer) -> String -> (Integer, Integer)
applyShuffle (a, b) line
  -- deal into new stack: pos -> size - 1 - pos.
  -- card = a * (size - 1 - pos') + b = -a*pos' + (a*(size-1)+b)
  -- a' = -a, b' = b - a (mod size)
  | line == "deal into new stack" =
      let a' = negate a
          b' = b + a' -- b' = b - a
      in (a' `mod` size, b' `mod` size)

  -- cut n: pos -> pos - n.
  -- card = a * (pos' + n) + b = a*pos' + (an + b)
  -- a' = a, b' = b + an (mod size)
  | "cut" `isPrefixOf` line =
      let n = read (last (words line)) :: Integer
          b' = b + n * a
      in (a, b' `mod` size)

  -- deal with increment n: pos -> pos * n^-1.
  -- card = a * (pos' * n) + b = (an)*pos' + b
  -- a' = an, b' = b.
  -- *However*, Python code calculates a' = a * n^-1 and b' = b. We replicate Python.
  | "deal with increment" `isPrefixOf` line =
      let n = read (last (words line)) :: Integer
          inv = modInverse n size
          a' = a * inv
      in (a' `mod` size, b) -- b remains unchanged as per Python's second loop

  | otherwise = (a, b) -- Ignore empty or invalid lines

main :: IO ()
main = do
    content <- readFile "input.txt"
    let instructions = lines content

    -- Calculate the transformation (a, b) for a single shuffle pass.
    -- Initial state is identity: card = 1 * pos + 0
    let (a, b) = foldl' applyShuffle (1, 0) instructions

    -- Calculate the transformation (A, B) after 'iter' passes using exponentiation by squaring.
    -- If f(pos) = a*pos + b represents the transformation card = f(pos), then
    -- f^k(pos) = a^k * pos + b * (a^k - 1) / (a - 1).
    -- A = a^iter mod size
    -- B = b * (a^iter - 1) * (a - 1)^(-1) mod size
    let a_k = powMod a iter size

    -- Calculate the geometric series sum part B carefully, handling a=1 case.
    -- We need (a^k - 1) * (a-1)^-1 mod size.
    let b_k = if a == 1 then
                  -- If a = 1, card = pos + b, repeated k times: card = pos + k*b
                  (b * iter) `mod` size
              else
                  -- Geometric series sum: (a^k - 1) / (a - 1)
                  let numerator = (a_k - 1 + size) `mod` size -- (a^k - 1) mod size, ensuring positive
                      denominator_inv = modInverse (a - 1 + size) size -- (a - 1)^(-1) mod size, ensuring positive base
                      geomSum = (numerator * denominator_inv) `mod` size
                  in (b * geomSum) `mod` size -- B = b * geomSum mod size

    -- The final goal is to find the card number at targetPos after 'iter' shuffles.
    -- card = A * targetPos + B mod size
    let finalCard = (a_k * targetPos + b_k) `mod` size

    print finalCard
