
import Data.List (intercalate)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    content <- B.readFile "input.txt"
    let imageData = unpack content
        width = 25
        height = 6
        layerSize = width * height
        layers = chunks layerSize imageData
        finalImage = foldl mergeLayers (replicate layerSize '2') layers
    putStrLn "Decoded image:"
    mapM_ (putStrLn . map pixelToChar) (chunks width finalImage)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

mergeLayers :: String -> String -> String
mergeLayers final layer = zipWith merge final layer
  where merge '2' p = p
        merge f _ = f

pixelToChar :: Char -> Char
pixelToChar '0' = ' '
pixelToChar '1' = '#'
pixelToChar _   = ' '
