import System.IO
import Data.List
import Data.Maybe

main :: IO ()
main = do
    let mfcsamOutput = [ ("children", 3)
                       , ("cats", 7)
                       , ("samoyeds", 2)
                       , ("pomeranians", 3)
                       , ("akitas", 0)
                       , ("vizslas", 0)
                       , ("goldfish", 5)
                       , ("trees", 3)
                       , ("cars", 2)
                       , ("perfumes", 1)
                       ]
    
    contents <- readFile "input.txt"
    let aunts = map parseAunt (lines contents)
    
    let matchingAunt = find (matchesMFCSAM mfcsamOutput) aunts
    
    case matchingAunt of
        Just aunt -> print (auntNumber aunt)
        Nothing -> putStrLn "No matching Aunt Sue found."

data Aunt = Aunt { auntNumber :: Int, properties :: [(String, Int)] }

parseAunt :: String -> Aunt
parseAunt line = 
    let parts = words (map (\c -> if c == ':' || c == ',' then ' ' else c) line)
        number = read (parts !! 1) :: Int
        props = [(parts !! 2, read (parts !! 3) :: Int), (parts !! 4, read (parts !! 5) :: Int), (parts !! 6, read (parts !! 7) :: Int)]
    in Aunt number props

matchesMFCSAM :: [(String, Int)] -> Aunt -> Bool
matchesMFCSAM mfcsamOutput aunt = all (\(k, v) -> fromMaybe v (lookup k (properties aunt)) == v) mfcsamOutput