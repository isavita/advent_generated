
import System.IO
import Data.List
import qualified Data.Map as M
import Text.Printf
import Data.Maybe (fromMaybe)

data Gate = Gate String String String deriving (Show, Eq)

makeKey :: String -> String -> String -> (String, String, String)
makeKey a op b = (min a b, op, max a b)

doSwap :: [(Gate, String)] -> String -> String -> [(Gate, String)]
doSwap gates s1 s2 = 
    map (\(g, out) -> if out == s1 then (g, s2) else if out == s2 then (g, s1) else (g, out)) gates

solve :: [(Gate, String)] -> [String] -> [String]
solve currentGates pairs
    | length pairs == 8 = pairs
    | otherwise =
        let 
            fwd = M.fromList [(out, g) | (g, out) <- currentGates]
            rev = M.fromList [(makeKey a op b, out) | (Gate op a b, out) <- currentGates]
            numZ = length $ filter (\(_, out) -> head out == 'z') currentGates
            
            check i carry
                | i == numZ - 1 = []
                | i == 0 =
                    let xi = printf "x%02d" i :: String
                        yi = printf "y%02d" i :: String
                        zi = printf "z%02d" i :: String
                        a0 = M.lookup (makeKey xi "XOR" yi) rev
                        c0 = M.lookup (makeKey xi "AND" yi) rev
                    in if a0 /= Just zi 
                       then [zi, fromMaybe "" a0]
                       else check (i + 1) (fromMaybe "" c0)
                | otherwise =
                    let xi = printf "x%02d" i :: String
                        yi = printf "y%02d" i :: String
                        zi = printf "z%02d" i :: String
                        bitKey = makeKey xi "XOR" yi
                        bit = M.lookup bitKey rev
                    in case bit of
                        Just b -> 
                            let addr = M.lookup (makeKey b "XOR" carry) rev
                            in case addr of
                                Nothing ->
                                    let Gate _ ga gb = fwd M.! zi
                                    in if M.member (makeKey ga "XOR" carry) rev
                                       then [b, ga]
                                       else [b, gb]
                                Just a | a /= zi -> [a, zi]
                                Just a ->
                                    let c1 = M.lookup (makeKey xi "AND" yi) rev
                                        c2 = M.lookup (makeKey b "AND" carry) rev
                                    in case (c1, c2) of
                                        (Just v1, Just v2) ->
                                            let nextC = M.lookup (makeKey v1 "OR" v2) rev
                                            in check (i + 1) (fromMaybe "" nextC)
                                        _ -> []
                        Nothing -> []

            toSwap = check 0 ""
        in if null toSwap then pairs else solve (doSwap currentGates (toSwap!!0) (toSwap!!1)) (toSwap ++ pairs)

parseLine :: String -> (Gate, String)
parseLine line =
    let ws = words line
    in (Gate (ws !! 1) (ws !! 0) (ws !! 2), ws !! 4)

splitOn :: String -> String -> [String]
splitOn delim str = case search delim str of
    Nothing -> [str]
    Just i -> take i str : splitOn delim (drop (i + length delim) str)
    where search d s = findIndex (isPrefixOf d) (tails s)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let parts = splitOn "\n\n" content
    let gStr = if length parts > 1 then parts !! 1 else ""
    let gates = map parseLine $ filter (not . null) $ lines gStr
    let result = sort $ solve gates []
    putStrLn $ intercalate "," result

