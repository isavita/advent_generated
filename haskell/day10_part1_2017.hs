
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.List (foldl')

main :: IO ()
main = do
  txt <- B.readFile "input.txt"
  let lens = map (read . B.unpack) $ B.split ',' txt
  let nums = [0..255]
  let (nums',_,_) = foldl' step (nums,0,0) lens
  print $ product $ take 2 nums'

step :: ([Int],Int,Int) -> Int -> ([Int],Int,Int)
step (nums,pos,skip) len =
    let n       = length nums
        indices = take len $ iterate (\x -> (x+1) `mod` n) pos
        rev     = reverse [nums !! i | i <- indices]
        nums'   = foldl' (\m (i,v) -> set m i v) nums (zip indices rev)
        pos'    = (pos + len + skip) `mod` n
        skip'   = skip + 1
    in (nums', pos', skip')
  where
    set xs i v = let (a,b:bs) = splitAt i xs in a ++ (v:bs)
