
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

data Rule = Rule { condition :: Maybe Text, destination :: Text } deriving (Show)
type Workflow = [Rule]
type Workflows = M.Map Text Workflow
type Part = M.Map Text Int

parseRule :: Text -> Rule
parseRule t = case T.splitOn ":" t of
  [cond, dest] -> Rule (Just cond) dest
  [dest]       -> Rule Nothing dest
  _            -> error "Invalid rule"

parseWorkflow :: Text -> (Text, Workflow)
parseWorkflow line = (name, rules)
  where
    [name, rest] = T.splitOn "{" $ T.init line
    rules = map parseRule $ T.splitOn "," rest

parsePart :: Text -> Part
parsePart line = M.fromList $ map parseRating $ T.splitOn "," $ T.drop 1 $ T.init line
  where
    parseRating rating = case T.splitOn "=" rating of
      [k, v] -> (k, read $ T.unpack v)
      _      -> error "Invalid rating"

parseInput :: Text -> (Workflows, [Part])
parseInput input = (workflows, parts)
  where
    [workflowsStr, partsStr] = T.splitOn "\n\n" input
    workflows = M.fromList $ map parseWorkflow $ T.lines workflowsStr
    parts = map parsePart $ T.lines partsStr

evalCondition :: Part -> Text -> Bool
evalCondition part cond = case T.splitOn "<" cond of
    [var, val] -> maybe False (< read (T.unpack val)) (M.lookup var part)
    _ -> case T.splitOn ">" cond of
        [var, val] -> maybe False (> read (T.unpack val)) (M.lookup var part)
        _ -> False

processPart :: Workflows -> Part -> Bool
processPart workflows part = go "in"
    where
        go workflowName = case M.lookup workflowName workflows of
            Just rules -> case dropWhile (not . checkCondition) rules of
                (Rule _ "A":_) -> True
                (Rule _ "R":_) -> False
                (Rule _ nextWorkflow:_) -> go nextWorkflow
                _ -> False
            Nothing -> False
        checkCondition (Rule Nothing _) = True
        checkCondition (Rule (Just cond) _) = evalCondition part cond

main :: IO ()
main = do
  input <- TIO.readFile "input.txt"
  let (workflows, parts) = parseInput input
  let acceptedParts = filter (processPart workflows) parts
  let totalHeatLoss = sum $ map (sum . M.elems) acceptedParts
  print totalHeatLoss
