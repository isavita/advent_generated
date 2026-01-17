
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    s <- readFile "input.txt"
    let (gL, rest) = break null (lines s)
        grid = M.fromList [((r, c), v) | (r, row) <- zip [0..] gL, (c, v) <- zip [0..] row]
        robot = head [p | (p, v) <- M.toList grid, v == '@']
        step (p, g) m =
            let d = case m of '^' -> (-1, 0); 'v' -> (1, 0); '<' -> (0, -1); '>' -> (0, 1); _ -> (0, 0)
                nxt = (fst p + fst d, snd p + snd d)
                findE p' = case M.lookup p' g of 
                             Just 'O' -> findE (fst p' + fst d, snd p' + snd d)
                             v -> (p', v)
            in if d == (0, 0) then (p, g) else case M.lookup nxt g of
                Just '.' -> (nxt, M.insert nxt '@' (M.insert p '.' g))
                Just 'O' -> case findE nxt of
                    (ep, Just '.') -> (nxt, M.insert ep 'O' (M.insert nxt '@' (M.insert p '.' g)))
                    _ -> (p, g)
                _ -> (p, g)
        (_, fG) = foldl step (robot, grid) (concat rest)
    print $ sum [r * 100 + c | ((r, c), 'O') <- M.toList fG]

