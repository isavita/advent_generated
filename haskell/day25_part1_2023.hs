
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.List (foldl')
import Data.Maybe (isNothing)

type Graph = M.Map String (S.Set String)

parse :: String -> Graph
parse = foldl' (\g l -> let (u, _:r) = break (==':') l in foldl' (add u) g (words r)) M.empty . lines
  where add u g v = M.insertWith S.union v (S.singleton u) $ M.insertWith S.union u (S.singleton v) g

getPath :: Graph -> String -> String -> Maybe [(String, String)]
getPath g s t = bfs (Q.singleton (s, [])) (S.singleton s)
  where
    bfs Q.Empty _ = Nothing
    bfs ((c, p) Q.:<| r) v
      | c == t = Just (reverse p)
      | otherwise =
          let ns = M.findWithDefault S.empty c g
              uv = S.filter (`S.notMember` v) ns
              nexts = [(n, (c, n):p) | n <- S.toList uv]
          in bfs (r Q.>< Q.fromList nexts) (S.union v uv)

removeEdges :: Graph -> [(String, String)] -> Graph
removeEdges = foldl' (\g (u, v) -> M.adjust (S.delete u) v (M.adjust (S.delete v) u g))

reachable :: Graph -> String -> S.Set String
reachable g s = go (S.singleton s) [s]
  where
    go v [] = v
    go v (c:cs) =
      let ns = S.filter (`S.notMember` v) (M.findWithDefault S.empty c g)
      in go (S.union v ns) (S.toList ns ++ cs)

solve :: Graph -> Int
solve g =
  let ks = M.keys g
      s = head ks
      total = M.size g
      check t =
        let g' = foldl' (\acc _ -> maybe acc (removeEdges acc) (getPath acc s t)) g [1..3]
        in if isNothing (getPath g' s t)
           then Just (S.size (reachable g' s))
           else Nothing
      sz = head [res | t <- tail ks, Just res <- [check t]]
  in sz * (total - sz)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let graph = parse content
  print $ solve graph

