
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  input <- B.readFile "input.txt"
  let decompressedLength = decompress input 0 (B.length input)
  print decompressedLength

decompress :: B.ByteString -> Int -> Int -> Int
decompress input start end = go start 0
  where
    go i acc
      | i >= end = acc
      | otherwise =
        case findMarker i of
          Just (markerStart, charCount, repeatCount) ->
            let nextIndex = markerStart + length (show charCount) + length (show repeatCount) + 3
            in go (nextIndex + charCount) (acc + repeatCount * decompress input nextIndex (nextIndex + charCount))
          Nothing -> go (i + 1) (acc + 1)

    findMarker i =
      let remaining = B.drop i input
      in case B.uncons remaining of
        Just ('(', rest) ->
          case B.break (== 'x') rest of
            (charCountStr, rest2) ->
              case B.uncons rest2 of
                Just ('x', rest3) ->
                  case B.break (== ')') rest3 of
                    (repeatCountStr, rest4) ->
                      case B.uncons rest4 of
                        Just (')', _) ->
                          let charCount = read (B.unpack charCountStr) :: Int
                              repeatCount = read (B.unpack repeatCountStr) :: Int
                          in Just (i, charCount, repeatCount)
                        _ -> Nothing
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
