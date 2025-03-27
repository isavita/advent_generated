
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (foldl1')
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data SnailNumber = Regular Int | Pair SnailNumber SnailNumber deriving (Eq, Show)

magnitude :: SnailNumber -> Int
magnitude (Regular v) = v
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

addLeft :: SnailNumber -> Int -> SnailNumber
addLeft (Regular v) x = Regular (v + x)
addLeft (Pair l r) x = Pair (addLeft l x) r

addRight :: SnailNumber -> Int -> SnailNumber
addRight (Regular v) x = Regular (v + x)
addRight (Pair l r) x = Pair l (addRight r x)

explode' :: Int -> SnailNumber -> Maybe (SnailNumber, Int, Int)
explode' depth sn = case sn of
    Regular _ -> Nothing
    Pair (Regular lval) (Regular rval) | depth >= 4 -> Just (Regular 0, lval, rval)
    Pair l r ->
        case explode' (depth + 1) l of
            Just (newL, leftAdd, rightAdd) ->
                let !newR = if rightAdd > 0 then addLeft r rightAdd else r
                in Just (Pair newL newR, leftAdd, 0)
            Nothing ->
                case explode' (depth + 1) r of
                    Just (newR, leftAdd, rightAdd) ->
                        let !newL = if leftAdd > 0 then addRight l leftAdd else l
                        in Just (Pair newL newR, 0, rightAdd)
                    Nothing -> Nothing

explode :: SnailNumber -> Maybe SnailNumber
explode sn = case explode' 0 sn of
    Just (newSn, _, _) -> Just newSn
    Nothing -> Nothing

split :: SnailNumber -> Maybe SnailNumber
split (Regular v)
    | v >= 10 = Just (Pair (Regular (v `div` 2)) (Regular ((v + 1) `div` 2)))
    | otherwise = Nothing
split (Pair l r) =
    case split l of
        Just newL -> Just (Pair newL r)
        Nothing ->
            case split r of
                Just newR -> Just (Pair l newR)
                Nothing -> Nothing

reduce :: SnailNumber -> SnailNumber
reduce sn =
    case explode sn of
        Just explodedSn -> reduce explodedSn
        Nothing -> case split sn of
            Just splitSn -> reduce splitSn
            Nothing -> sn

add :: SnailNumber -> SnailNumber -> SnailNumber
add sn1 sn2 = reduce (Pair sn1 sn2)

parseSnail :: String -> Maybe (SnailNumber, String)
parseSnail ('[' : rest) = do
    (left, rest') <- parseSnail rest
    guard (not (null rest') && head rest' == ',')
    (right, rest'') <- parseSnail (tail rest')
    guard (not (null rest'') && head rest'' == ']')
    Just (Pair left right, tail rest'')
parseSnail s@(c : _) | isDigit c =
    let (digits, rest) = span isDigit s
    in case readMaybe digits of
        Just val -> Just (Regular val, rest)
        Nothing -> Nothing
parseSnail _ = Nothing

parseLine :: String -> Maybe SnailNumber
parseLine s = case parseSnail (filter (/= ' ') s) of
    Just (sn, "") -> Just sn
    _ -> Nothing

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let sns = mapMaybe parseLine (lines contents)
    if null sns
        then putStrLn "No snailfish numbers found in the file."
        else do
            let !finalSum = foldl1' add sns
            print (magnitude finalSum)

