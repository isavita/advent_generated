
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Maybe (mapMaybe)
import Control.Monad (forM_, when)
import Data.Array.IO (IOUArray, newArray, writeArray, getElems)

data Action = On | Off deriving (Eq)

data RebootStep = RebootStep {
    action :: Action,
    xMin, xMax, yMin, yMax, zMin, zMax :: !Int
}

parseRange :: T.Text -> Maybe (Int, Int)
parseRange txt =
    case T.splitOn ".." (T.drop 2 txt) of
        [startStr, endStr] -> do
            (start, _) <- either (const Nothing) Just $ TR.signed TR.decimal startStr
            (end, _)   <- either (const Nothing) Just $ TR.signed TR.decimal endStr
            Just (start, end)
        _ -> Nothing

parseRebootStep :: T.Text -> Maybe RebootStep
parseRebootStep line =
    case T.words line of
        [actionStr, coordsStr] -> do
            act <- case actionStr of
                       "on"  -> Just On
                       "off" -> Just Off
                       _     -> Nothing
            case T.splitOn "," coordsStr of
                [xPart, yPart, zPart] -> do
                    (xS, xE) <- parseRange xPart
                    (yS, yE) <- parseRange yPart
                    (zS, zE) <- parseRange zPart
                    Just $ RebootStep act xS xE yS yE zS zE
                _ -> Nothing
        _ -> Nothing

main :: IO ()
main = do
    contents <- TIO.readFile "input.txt"
    let steps = mapMaybe parseRebootStep $ T.lines contents
        minCoord = -50
        maxCoord = 50
        size = maxCoord - minCoord + 1
        offset = -minCoord
        bounds = ((0, 0, 0), (size - 1, size - 1, size - 1))

    grid <- newArray bounds False :: IO (IOUArray (Int, Int, Int) Bool)

    forM_ steps $ \step -> do
        let valid = xMin step >= minCoord && xMax step <= maxCoord &&
                    yMin step >= minCoord && yMax step <= maxCoord &&
                    zMin step >= minCoord && zMax step <= maxCoord
        when valid $ do
            let val = action step == On
            forM_ [xMin step .. xMax step] $ \x ->
              forM_ [yMin step .. yMax step] $ \y ->
                forM_ [zMin step .. zMax step] $ \z ->
                  writeArray grid (x + offset, y + offset, z + offset) val

    onCount <- length . filter id <$> getElems grid
    print onCount

