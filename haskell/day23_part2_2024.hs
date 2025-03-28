
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sort, intercalate)
import Data.Maybe (fromMaybe)
import System.IO (readFile)
import Control.Monad (guard) -- Used for filtering in list comprehensions if needed

-- Represents the graph as an adjacency map (Node -> Set of Neighbors)
type Graph = M.Map String (S.Set String)
type Clique = S.Set String

-- Parses a single line "node1-node2" into a tuple.
-- Returns Nothing if the format is invalid.
parseLine :: String -> Maybe (String, String)
parseLine line = case break (== '-') line of
                   (a, '-':b) | not (null a) && not (null b) -> Just (a, b)
                   _                                        -> Nothing

-- Builds the undirected graph from a list of edges.
buildGraph :: [(String, String)] -> Graph
buildGraph edges = foldr addEdge M.empty edges
  where
    -- Adds an edge (u, v) to the graph, ensuring connections in both directions.
    addEdge :: (String, String) -> Graph -> Graph
    addEdge (u, v) graph = M.insertWith S.union v (S.singleton u) $
                           M.insertWith S.union u (S.singleton v) graph

-- Generates cliques of size k+1 from cliques of size k.
-- A new clique C U {n} is formed if n is connected to all nodes in C.
-- It leverages set intersections for efficiency.
generateNextCliques :: Graph -> S.Set Clique -> S.Set Clique
generateNextCliques graph cliquesK = S.unions $ S.map findExtensions cliquesK
  where
    -- For a given clique, find all nodes that can extend it.
    findExtensions :: Clique -> S.Set Clique
    findExtensions clique =
      let -- Find nodes connected to *all* nodes currently in the clique.
          -- Start with neighbours of one node, then intersect with neighbours of others.
          candidateNodes = case S.minView clique of -- Get an arbitrary element 'firstNode'
                             Nothing -> M.keysSet graph -- Base case: if clique is empty, all nodes are candidates (for size 1)
                             Just (firstNode, rest) ->
                                 let initialNeighbors = fromMaybe S.empty (M.lookup firstNode graph)
                                 in S.foldl' (\commonNs node -> S.intersection commonNs (fromMaybe S.empty (M.lookup node graph)))
                                             initialNeighbors
                                             rest

          -- Filter out nodes that are already in the current clique.
          validAdditions = S.filter (`S.notMember` clique) candidateNodes

          -- Create the new cliques by adding each valid node.
          -- Using S.map automatically handles uniqueness of the resulting cliques.
      in S.map (`S.insert` clique) validAdditions


-- Finds the largest clique in the graph using an iterative approach.
-- Starts with cliques of size 1 (single nodes) and repeatedly generates
-- larger cliques until no more can be found. Keeps track of the largest clique seen.
findMaxClique :: Graph -> Clique
findMaxClique graph
  | M.null graph = S.empty -- Handle empty graph
  | otherwise = findCliqueIter initialCliques initialMax
  where
    nodes = M.keysSet graph
    -- Start with cliques of size 1 (each node is a clique).
    initialCliques = S.map S.singleton nodes
    -- Initial maximum clique is just one of the single nodes (if graph not empty).
    initialMax = if S.null initialCliques then S.empty else S.elemAt 0 initialCliques

    -- Iterative helper function:
    -- currentCliques: Set of cliques of the current size k
    -- maxCliqueFound: The largest clique found in any previous iteration
    findCliqueIter :: S.Set Clique -> Clique -> Clique
    findCliqueIter currentCliques maxCliqueFound
      -- If no cliques were generated at this level, the largest found previously is the maximum.
      | S.null currentCliques = maxCliqueFound
      | otherwise =
          let -- Generate potential cliques for the next size level (k+1).
              nextCliques = generateNextCliques graph currentCliques

              -- Determine the representative clique from the *current* level to compare sizes.
              -- All cliques in currentCliques have the same size (k).
              currentLevelMax = S.elemAt 0 currentCliques

              -- Update the overall maximum clique found so far.
              updatedMax = if S.size currentLevelMax > S.size maxCliqueFound
                              then currentLevelMax
                              else maxCliqueFound

              -- Recurse with the next level cliques and the updated maximum.
          in findCliqueIter nextCliques updatedMax

-- Main entry point.
main :: IO ()
main = do
    -- Read input file
    input <- readFile "input.txt"
    let edgeLines = lines input

    -- Parse lines into edges, filtering out any potential invalid lines
    let maybeEdges = map parseLine edgeLines
    let edges = [e | Just e <- maybeEdges] -- Extract valid edges

    -- Build the graph representation
    let graph = buildGraph edges

    -- Find the largest clique
    let maxClique = findMaxClique graph

    -- Format the result: sort node names alphabetically and join with commas
    let password = intercalate "," . sort . S.toList $ maxClique

    -- Print the final password to standard output
    putStrLn password
