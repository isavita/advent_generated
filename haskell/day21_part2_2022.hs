
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ratio ((%), numerator)
import Data.Maybe (fromJust)

type ValueType = Rational
data MonkeyExpr = Value ValueType | Operation String String String deriving (Show)
type MonkeyMap = Map String MonkeyExpr

parseLine :: String -> (String, MonkeyExpr)
parseLine line =
  let (name, rest) = break (== ':') line
      definition = drop 2 rest
      parts = words definition
  in (name, case parts of
              [numStr]   -> Value (fromInteger (read numStr))
              [l, op, r] -> Operation l op r
              _          -> error $ "Invalid line format: " ++ line)

parseInput :: String -> MonkeyMap
parseInput = Map.fromList . map parseLine . lines

applyOp :: String -> ValueType -> ValueType -> Maybe ValueType
applyOp "+" a b = Just (a + b)
applyOp "-" a b = Just (a - b)
applyOp "*" a b = Just (a * b)
applyOp "/" a b = if b == 0 then Nothing else Just (a / b)
applyOp _   _ _ = Nothing

solveMemo :: MonkeyMap -> Map String (Maybe ValueType) -> String -> (Maybe ValueType, Map String (Maybe ValueType))
solveMemo monkeys memo name
  | name == "humn" = (Nothing, memo)
  | otherwise = case Map.lookup name memo of
      Just result -> (result, memo)
      Nothing ->
        let (result, newMemo) = case Map.lookup name monkeys of
              Nothing -> error $ "Monkey not found: " ++ name
              Just (Value v) -> (Just v, memo)
              Just (Operation l op r) ->
                let (maybeL, memo1) = solveMemo monkeys memo l
                    (maybeR, memo2) = solveMemo monkeys memo1 r
                    currentResult = do
                      lv <- maybeL
                      rv <- maybeR
                      applyOp op lv rv
                in (currentResult, memo2)
            finalMemo = Map.insert name result newMemo
        in (result, finalMemo)

invertOpLeft :: String -> ValueType -> ValueType -> ValueType
invertOpLeft "+" target knownR = target - knownR
invertOpLeft "-" target knownR = target + knownR
invertOpLeft "*" target knownR = target / knownR
invertOpLeft "/" target knownR = target * knownR
invertOpLeft _   _      _      = error "Invalid op for left inversion"

invertOpRight :: String -> ValueType -> ValueType -> ValueType
invertOpRight "+" target knownL = target - knownL
invertOpRight "-" target knownL = knownL - target
invertOpRight "*" target knownL = target / knownL
invertOpRight "/" target knownL = knownL / target
invertOpRight _   _      _      = error "Invalid op for right inversion"

expect :: MonkeyMap -> Map String (Maybe ValueType) -> String -> ValueType -> ValueType
expect monkeys solvedValues name target
  | name == "humn" = target
  | otherwise = case Map.lookup name monkeys of
      Just (Operation l op r) ->
        let maybeL = Map.findWithDefault Nothing l solvedValues
            maybeR = Map.findWithDefault Nothing r solvedValues
        in case (maybeL, maybeR) of
             (Nothing, Just rv) -> expect monkeys solvedValues l (invertOpLeft op target rv)
             (Just lv, Nothing) -> expect monkeys solvedValues r (invertOpRight op target lv)
             _ -> error "Expect: Invalid state - both or neither child solved, or humn not in tree"
      _ -> error "Expect: Expected operation monkey"

main :: IO ()
main = do
  input <- readFile "input.txt"
  let monkeys = parseInput input
  case Map.lookup "root" monkeys of
    Just (Operation l _ r) -> do
      let initialMemo = Map.singleton "humn" Nothing
      let (_, memo1) = solveMemo monkeys initialMemo l
      let (_, finalMemo) = solveMemo monkeys memo1 r

      let maybeLVal = Map.findWithDefault Nothing l finalMemo
      let maybeRVal = Map.findWithDefault Nothing r finalMemo

      let result = case (maybeLVal, maybeRVal) of
                     (Nothing, Just rv) -> expect monkeys finalMemo l rv
                     (Just lv, Nothing) -> expect monkeys finalMemo r lv
                     _ -> error "Main: Root children state invalid after solve"

      print (numerator result) -- Assumes the final answer is an integer representable by Rational's numerator
    _ -> error "Main: Root monkey not found or not an operation"

