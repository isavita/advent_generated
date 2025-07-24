
{-# LANGUAGE RecordWildCards #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Data.Bits (xor)

data Module = Broadcaster [String]
            | FlipFlop Bool [String]
            | Conjunction (M.Map String Bool) [String]
            deriving (Show, Eq)

type Connections = M.Map String Module
data Pulse = Low | High deriving (Show, Eq)

parseLine :: String -> (String, Module)
parseLine s = case words $ map (\c -> if c == ',' then ' ' else c) s of
    ("broadcaster":_:xs) -> ("broadcaster", Broadcaster xs)
    (x:_:xs) -> let name = tail x
                    outs = xs
                in case head x of
                    '%' -> (name, FlipFlop False outs)
                    '&' -> (name, Conjunction M.empty outs)
                    _   -> error "Invalid module type"

completeWatches :: Connections -> Connections
completeWatches conns = M.mapWithKey updateConj conns
  where
    updateConj name (Conjunction _ outs) =
        let inputs = M.keys $ M.filter (any (==name) . getOutputs) conns
        in Conjunction (M.fromList [(i, False) | i <- inputs]) outs
    updateConj _ m = m
    getOutputs (Broadcaster outs) = outs
    getOutputs (FlipFlop _ outs) = outs
    getOutputs (Conjunction _ outs) = outs

simulatePress :: Connections -> M.Map String Int -> Int -> (Connections, M.Map String Int, Bool)
simulatePress conns loops press = go [(Low, "button", "broadcaster")] conns loops False
  where
    go [] conns' loops' found = (conns', loops', found)
    go ((pulse, from, to):rest) conns' loops' found
        | to == "rx" && pulse == Low = (conns', loops', True)
        | otherwise = case M.lookup to conns' of
            Nothing -> go rest conns' loops' found
            Just m -> case m of
                Broadcaster outs ->
                    let newPulses = [(pulse, to, o) | o <- outs]
                    in go (rest ++ newPulses) conns' loops' found
                FlipFlop state outs ->
                    case pulse of
                        High -> go rest conns' loops' found
                        Low ->
                            let newState = not state
                                newPulses = [(if newState then High else Low, to, o) | o <- outs]
                                newConns = M.insert to (FlipFlop newState outs) conns'
                            in go (rest ++ newPulses) newConns loops' found
                Conjunction memory outs ->
                    let newMemory = M.insert from (pulse == High) memory
                        allHigh = and $ M.elems newMemory
                        outPulse = if allHigh then Low else High
                        newPulses = [(outPulse, to, o) | o <- outs]
                        newConns = M.insert to (Conjunction newMemory outs) conns'
                        newLoops = if not allHigh && M.lookup to loops' == Just (-1)
                                   then M.insert to press loops'
                                   else loops'
                    in go (rest ++ newPulses) newConns newLoops found

findLoopLengths :: Connections -> M.Map String Int -> Int -> M.Map String Int
findLoopLengths conns loops press
    | all (/= -1) (M.elems loops) = loops
    | otherwise =
        let (newConns, newLoops, _) = simulatePress conns loops press
        in findLoopLengths newConns newLoops (press + 1)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let lines' = lines content
        parsed = map parseLine lines'
        conns = completeWatches $ M.fromList parsed
    
    let rxInput = head [name | (name, m) <- M.toList conns, "rx" `elem` getOutputs m]
        (Conjunction memory _) = fromMaybe (error "Invalid") $ M.lookup rxInput conns
        initialLoops = M.fromList [(k, -1) | k <- M.keys memory]
        finalLoops = findLoopLengths conns initialLoops 1
    
    print $ product $ M.elems finalLoops
  where
    getOutputs (Broadcaster outs) = outs
    getOutputs (FlipFlop _ outs) = outs
    getOutputs (Conjunction _ outs) = outs
