
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl', sortBy, head)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import System.IO (readFile)

type Pos = (Int, Int)
data Dir = U | D | L | R deriving (Eq, Ord, Show)
data Turn = TLeft | TStraight | TRight deriving (Eq, Ord, Show)
data Cart = Cart { pos :: Pos, dir :: Dir, nextTurn :: Turn, cartId :: Int } deriving (Show, Eq)

instance Ord Cart where
    compare c1 c2 = comparing (\c -> (snd $ pos c, fst $ pos c)) c1 c2

delta :: Dir -> Pos
delta U = (0, -1)
delta D = (0, 1)
delta L = (-1, 0)
delta R = (1, 0)

turnLeft :: Dir -> Dir
turnLeft U = L
turnLeft D = R
turnLeft L = D
turnLeft R = U

turnRight :: Dir -> Dir
turnRight U = R
turnRight D = L
turnRight L = U
turnRight R = D

cycleTurn :: Turn -> Turn
cycleTurn TLeft = TStraight
cycleTurn TStraight = TRight
cycleTurn TRight = TLeft

getTrackAndDir :: Char -> Maybe (Char, Dir)
getTrackAndDir '^' = Just ('|', U)
getTrackAndDir 'v' = Just ('|', D)
getTrackAndDir '<' = Just ('-', L)
getTrackAndDir '>' = Just ('-', R)
getTrackAndDir _   = Nothing

parseInput :: String -> (Map.Map Pos Char, Map.Map Int Cart)
parseInput contents = (tracks, carts)
  where
    indexedLines = zip [0..] (lines contents)
    cells = concat [ [((x, y), c) | (x, c) <- zip [0..] line] | (y, line) <- indexedLines ]

    initialData = mapMaybe processCell cells
      where
        processCell (p, c)
          | Just (trackChar, cartDir) <- getTrackAndDir c = Just (p, Left (trackChar, cartDir))
          | c `elem` "|-/\\+" = Just (p, Right c)
          | otherwise = Nothing

    tracks = Map.fromList $ mapMaybe (\(p, e) -> case e of
                                           Left (trackChar, _) -> Just (p, trackChar)
                                           Right trackChar     -> Just (p, trackChar)) initialData

    cartList = mapMaybe (\(idx, (p, e)) -> case e of
                                            Left (_, cartDir) -> Just (Cart p cartDir TLeft idx)
                                            Right _           -> Nothing) (zip [0..] initialData)

    carts = Map.fromList $ map (\c -> (cartId c, c)) cartList

moveCart :: Map.Map Pos Char -> Cart -> Cart
moveCart tracks cart@Cart{..} =
    let (dx, dy) = delta dir
        newPos = (fst pos + dx, snd pos + dy)
        track = tracks Map.! newPos
        (newDir, newNextTurn) = case track of
            '/'  -> (case dir of U -> R; D -> L; L -> D; R -> U, nextTurn)
            '\\' -> (case dir of U -> L; D -> R; L -> U; R -> D, nextTurn)
            '+'  -> case nextTurn of
                        TLeft     -> (turnLeft dir, cycleTurn nextTurn)
                        TStraight -> (dir, cycleTurn nextTurn)
                        TRight    -> (turnRight dir, cycleTurn nextTurn)
            _    -> (dir, nextTurn)
    in cart { pos = newPos, dir = newDir, nextTurn = newNextTurn }

tick :: Map.Map Pos Char -> Map.Map Int Cart -> Map.Map Int Cart
tick tracks carts = fst $ foldl' processCart (carts, Set.empty) sortedCartIds
  where
    sortedCartIds = map cartId $ sortBy (comparing pos) (Map.elems carts)

    processCart :: (Map.Map Int Cart, Set.Set Int) -> Int -> (Map.Map Int Cart, Set.Set Int)
    processCart acc@(currentCartsMap, removedIds) cid
      | Set.member cid removedIds = acc
      | otherwise =
          let currentCart = currentCartsMap Map.! cid
              movedCart = moveCart tracks currentCart
              newPos = pos movedCart

              collidingPartnerId = Map.foldrWithKey (\otherCid otherCart found ->
                                      if found == Nothing && otherCid /= cid && not (Set.member otherCid removedIds) && pos otherCart == newPos
                                      then Just otherCid
                                      else found) Nothing currentCartsMap

          in case collidingPartnerId of
               Just otherId ->
                 let newRemovedIds = Set.insert cid (Set.insert otherId removedIds)
                     newCartsMap = Map.delete cid (Map.delete otherId currentCartsMap)
                 in (newCartsMap, newRemovedIds)
               Nothing ->
                 let newCartsMap = Map.insert cid movedCart currentCartsMap
                 in (newCartsMap, removedIds)

simulate :: Map.Map Pos Char -> Map.Map Int Cart -> Pos
simulate tracks initialCarts = loop initialCarts
  where
    loop currentCarts =
        if Map.size currentCarts <= 1 then
            pos $ head $ Map.elems currentCarts
        else
            loop (tick tracks currentCarts)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let (tracks, initialCarts) = parseInput contents
    let finalPos = simulate tracks initialCarts
    putStrLn $ show (fst finalPos) ++ "," ++ show (snd finalPos)
