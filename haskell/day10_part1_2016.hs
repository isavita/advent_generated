
import Data.Map (Map, empty, insert, lookup, member, delete)
import Data.List (sort)

data Instruction = Value Int Int | Bot Int Int Int deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction line =
  case words line of
    ["value", val, "goes", "to", "bot", bot] -> Value (read val) (read bot)
    ["bot", bot, "gives", "low", "to", dest1, "and", "high", "to", dest2] ->
      let (destType1, destNum1) = parseDest dest1
          (destType2, destNum2) = parseDest dest2
       in Bot (read bot) destNum1 destNum2
    _ -> error ("Invalid instruction: " ++ line)

parseDest :: String -> (String, Int)
parseDest s =
  case words s of
    [t, n] -> (t, read n)
    _ -> error ("Invalid destination: " ++ s)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let instructions = map parseInstruction (lines contents)
  let (valueInstructions, botInstructions) = partition isValueInstruction instructions
  let initialChips = map (\(Value v b) -> (b, v)) valueInstructions
  solve initialChips botInstructions

isValueInstruction :: Instruction -> Bool
isValueInstruction (Value _ _) = True
isValueInstruction _ = False

solve :: [(Int, Int)] -> [Instruction] -> IO ()
solve initialChips botInstructions = do
  let bots = foldr (\(b,v) m -> insert b (sort [v]) m) empty initialChips
      bots' = runBots bots botInstructions
  let botNum = findBot 61 17 bots'
  print botNum


runBots :: Map Int [Int] -> [Instruction] -> Map Int [Int]
runBots bots instructions = go bots instructions
  where
    go currentBots [] = currentBots
    go currentBots (i:is) =
      case i of
        Value v b -> go (insert b (sort $ v : fromMaybe [] (lookup b currentBots)) currentBots) is
        Bot b low high ->
          let chips = fromMaybe [] (lookup b currentBots)
              in if length chips >= 2
                 then let lowChip = head chips
                          highChip = last chips
                          in go (delete b currentBots
                                 `insert` low (sort $ lowChip : fromMaybe [] (lookup low currentBots))
                                 `insert` high (sort $ highChip : fromMaybe [] (lookup high currentBots))
                                ) is
                 else go currentBots is

findBot :: Int -> Int -> Map Int [Int] -> Int
findBot val1 val2 bots =
  case filter (\(k,v) -> elem val1 v && elem val2 v) (toList bots) of
    [(k, _)] -> k
    _ -> error "Bot not found"

toList :: Map k a -> [(k, a)]
toList = Data.Map.toList
