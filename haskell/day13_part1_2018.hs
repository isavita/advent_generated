import Data.List
import Data.Maybe
import qualified Data.Map as Map

data Cart = Cart { x :: Int, y :: Int, direction :: Char, turn :: Int } deriving (Show, Eq)

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    let tracks = map (map (\c -> if c `elem` "<>^v" then if c == '<' || c == '>' then '-' else '|' else c)) input
    let carts = concatMap (\(y, line) -> mapMaybe (\(x, c) -> if c `elem` "<>^v" then Just (Cart x y c 0) else Nothing) (zip [0..] line)) (zip [0..] input)
    let (crashX, crashY) = findFirstCrash tracks carts
    putStrLn $ show crashX ++ "," ++ show crashY

findFirstCrash :: [[Char]] -> [Cart] -> (Int, Int)
findFirstCrash tracks carts = go carts
    where
        go carts = case findCollision carts of
            Just (x, y) -> (x, y)
            Nothing -> go (sortCarts (moveCarts tracks carts))

findCollision :: [Cart] -> Maybe (Int, Int)
findCollision carts = let positions = map (\c -> (x c, y c)) carts in find (\pos -> length (filter (== pos) positions) > 1) positions

sortCarts :: [Cart] -> [Cart]
sortCarts = sortBy (\c1 c2 -> compare (y c1, x c1) (y c2, x c2))

moveCarts :: [[Char]] -> [Cart] -> [Cart]
moveCarts tracks = map (moveCart tracks)

moveCart :: [[Char]] -> Cart -> Cart
moveCart tracks cart = let (newX, newY) = case direction cart of
                                                '>' -> (x cart + 1, y cart)
                                                '<' -> (x cart - 1, y cart)
                                                '^' -> (x cart, y cart - 1)
                                                'v' -> (x cart, y cart + 1)
                        in updateCart (tracks !! newY !! newX) (cart { x = newX, y = newY })

updateCart :: Char -> Cart -> Cart
updateCart track cart = case track of
    '/'  -> cart { direction = if direction cart == '>' then '^' else if direction cart == '<' then 'v' else if direction cart == '^' then '>' else '<' }
    '\\' -> cart { direction = if direction cart == '>' then 'v' else if direction cart == '<' then '^' else if direction cart == '^' then '<' else '>' }
    '+'  -> let newDir = case turn cart `mod` 3 of
                            0 -> if direction cart == '>' then '^' else if direction cart == '<' then 'v' else if direction cart == '^' then '<' else '>'
                            1 -> direction cart
                            2 -> if direction cart == '>' then 'v' else if direction cart == '<' then '^' else if direction cart == '^' then '>' else '<'
            in cart { direction = newDir, turn = turn cart + 1 }
    _    -> cart