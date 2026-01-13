
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.List (intercalate)
import Data.Char (isSpace)
import System.IO

data Row = Row { springs :: String, group :: [Int], groupLen :: Int }

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseRow :: String -> Row
parseRow line =
    let (s:g:_) = words line
        grp = map read $ splitComma g
    in Row s grp (length grp)
  where
    splitComma = foldr (\c (w:ws) -> if c == ',' then []:w:ws else (c:w):ws) [[]]

unfoldRow :: Row -> Int -> Row
unfoldRow (Row sp gr _) k =
    let newSpr = intercalate "?" (replicate k sp)
        newGrp = concat $ replicate k gr
    in Row newSpr newGrp (length newGrp)

countArrangements :: Row -> Integer
countArrangements row = fst $ go 0 0 0 M.empty
  where
    len = length (springs row)
    go :: Int -> Int -> Int -> Map (Int,Int,Int) Integer -> (Integer, Map (Int,Int,Int) Integer)
    go iS iG iC memo
      | iS == len =
          let val = if (iG == groupLen row && iC == 0) ||
                       (iG == groupLen row - 1 && iC == (group row !! iG))
                    then 1 else 0
          in (val, memo)
      | otherwise =
          case M.lookup key memo of
            Just v  -> (v, memo)
            Nothing ->
              let ch = springs row !! iS
                  (r1,m1) = if ch `elem` ".?"
                            then if iC == 0
                                 then go (iS+1) iG iC memo
                                 else if iG < groupLen row && iC == (group row !! iG)
                                      then go (iS+1) (iG+1) 0 memo
                                      else (0,memo)
                            else (0,memo)
                  (r2,m2) = if ch `elem` "#?"
                            then if iG < groupLen row && iC < (group row !! iG)
                                 then go (iS+1) iG (iC+1) m1
                                 else (0,m1)
                            else (0,m1)
                  total = r1 + r2
                  memo' = M.insert key total m2
              in (total, memo')
      where key = (iS,iG,iC)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let total = sum [ countArrangements (unfoldRow (parseRow (trim line)) 5)
                    | line <- lines content
                    , not (null (trim line))
                    ]
    print total
