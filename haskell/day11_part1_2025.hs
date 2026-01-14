
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import Data.List (foldl')

main :: IO ()
main = do
    txt <- T.readFile "input.txt"
    let gr = M.fromList . map (parse . T.words) . T.lines $ txt
    print (dfs gr "you" "out")
  where
    parse (x:xs) = (T.init x, xs)

dfs :: M.Map T.Text [T.Text] -> T.Text -> T.Text -> Int
dfs gr start end = go M.empty start
  where
    go seen v
      | v == end  = 1
      | otherwise = case M.lookup v gr of
          Nothing -> 0
          Just ns -> sum [ go (M.insert v True seen) n | n <- ns, not (M.member v seen) ]
