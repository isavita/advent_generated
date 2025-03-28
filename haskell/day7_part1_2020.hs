
{-# OPTIONS_GHC -O2 #-} -- Request optimization for efficiency

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf, isSuffixOf)

-- Type synonyms for clarity
type Color = String
-- Using a reverse graph for this problem: Map containedColor [containerColor]
type ReverseGraph = Map.Map Color [Color]

-- == Helper Functions ==

-- Safely removes a suffix from a list (String) if present.
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix sfx lst =
  let n = length lst
      m = length sfx
  in if n >= m && drop (n - m) lst == sfx
     then Just (take (n - m) lst)
     else Nothing

-- Removes a trailing dot from a string, if present. Otherwise returns original.
removeTrailingDot :: String -> String
removeTrailingDot s = case stripSuffix "." s of
                        Just s' -> s'
                        Nothing -> s

-- Finds the first occurrence of a non-empty substring and returns the parts
-- before and including/after the substring.
-- Returns (full string, "") if needle is not found or empty.
breakSubstring :: String -> String -> (String, String)
breakSubstring needle haystack
    | null needle = (haystack, "") -- Avoid issues with empty needle
    | otherwise   = go haystack 0
  where
    go hs idx
      | null hs = (haystack, "") -- Not found
      | needle `isPrefixOf` hs = (take idx haystack, hs)
      | otherwise = if null (tail hs)
                    then (haystack, "") -- Reached end without finding
                    else go (tail hs) (idx + 1)

-- Splits a string based on a delimiter string. Handles empty strings.
-- Similar to Python's string.split(delimiter).
splitOn :: String -> String -> [String]
splitOn delim str
    | null delim = map (:[]) str -- Split into characters if delim is empty
    | null str = [""]           -- Mimic Python's split on empty string
    | otherwise = go str
  where
    delimLen = length delim
    go s =
        let (before, after) = breakSubstring delim s
        -- Check if delimiter was actually found. If 'after' is empty and
        -- 'before' == s, it means the delimiter wasn't present in 's'.
        in if null after && before == s
             then [before] -- Return the remaining string as the last element
             else before : go (drop delimLen after)

-- == Parsing Logic ==

-- Parses a single rule line directly into the information needed for the reverse graph:
-- (containerColor, [list of contained colors]).
-- Returns Nothing if the line format is invalid.
parseRuleForReverse :: String -> Maybe (Color, [Color])
parseRuleForReverse line =
    let ws = words line
    in case span (/= "bags") ws of -- Split words before "bags"
        (containerWords, "bags":"contain":rest) ->
            let containerColor = unwords containerWords
            in if rest == ["no", "other", "bags."] -- Handle "no other bags." case
               then Just (containerColor, [])
               else Just (containerColor, parseContainedColors (unwords rest))
        _ -> Nothing -- Line doesn't match the expected "container bags contain content" structure

-- Parses the content part of a rule (string after "contain ")
-- and returns only the list of contained colors.
parseContainedColors :: String -> [Color]
parseContainedColors s = mapMaybe parseSingleContainedColor (splitOn ", " (removeTrailingDot s))

-- Parses a single item description (e.g., "2 muted yellow bags")
-- from the contents list and returns just the color ("muted yellow").
-- Returns Nothing if the format is invalid.
parseSingleContainedColor :: String -> Maybe Color
parseSingleContainedColor itemStr = case words itemStr of
    (_numStr:colorWords) -> -- We ignore the number (_numStr) for Part 1
        -- Take words representing the color, stopping before "bag" or "bags"
        let colorPart = takeWhile (\w -> not ("bag" `isPrefixOf` w)) colorWords
        in if null colorPart -- Check if color part is empty (malformed item)
           then Nothing
           else Just (unwords colorPart)
    _ -> Nothing -- Item string is empty or doesn't start with a number

-- == Graph Construction ==

-- Builds the reverse graph (map from a color to list of colors that can contain it)
-- from the parsed rules.
buildReverseGraph :: [(Color, [Color])] -> ReverseGraph
buildReverseGraph parsedRules = foldl addRuleEdges Map.empty parsedRules
  where
    -- Function to process one rule and update the reverse graph
    addRuleEdges :: ReverseGraph -> (Color, [Color]) -> ReverseGraph
    addRuleEdges graph (container, containedColors) =
        -- For each color C contained by 'container', add 'container' to the
        -- list of bags that can contain C in the reverse graph.
        foldl (\accGraph containedColor ->
                 -- Map.insertWith appends the new container to the existing list,
                 -- or creates a new list if the containedColor is not yet a key.
                 Map.insertWith (++) containedColor [container] accGraph
              ) graph containedColors

-- == Solver Logic ==

-- Finds all unique ancestor bag colors for a given target color using the reverse graph.
-- This performs a graph traversal (similar to Breadth-First Search) starting from
-- the bags that directly contain the target.
-- `graph`: The reverse graph (contained -> containers).
-- `toVisit`: A set of nodes (colors) whose parents (containers) need to be explored.
-- `visited`: A set of nodes (colors) that have already been identified as ancestors.
findAncestors :: ReverseGraph -> Set.Set Color -> Set.Set Color -> Set.Set Color
findAncestors graph toVisit visited =
    if Set.null toVisit then
        visited -- Base case: No more nodes to explore, return all found ancestors.
    else
        -- Find all direct parents (containers) for the current set of nodes 'toVisit'.
        -- Uses Map.findWithDefault to handle colors that are not contained by any other bag.
        let currentParents = Set.unions $ Set.map (\node -> Set.fromList $ Map.findWithDefault [] node graph) toVisit
        -- Identify the parents that haven't been visited yet. These are the new nodes to explore in the next step.
            newNodes = Set.difference currentParents visited
        -- Recursively call findAncestors with the new nodes to visit,
        -- adding these new nodes to the set of visited ancestors.
        in findAncestors graph newNodes (Set.union visited newNodes)

-- == Main Execution ==

main :: IO ()
main = do
    -- Read the content of the input file.
    contents <- readFile "input.txt"

    -- Parse each line of the input file into a (container, [contained colors]) format.
    -- Filter out any lines that don't parse correctly using mapMaybe.
    let parsedRules = mapMaybe parseRuleForReverse (lines contents)

    -- Construct the reverse graph from the parsed rules.
    let reverseGraph = buildReverseGraph parsedRules

    -- Define the target bag color we are interested in.
    let targetColor = "shiny gold"

    -- Find the initial set of bags that directly contain the target color.
    -- These are the starting points for our graph traversal.
    let directParents = Set.fromList $ Map.findWithDefault [] targetColor reverseGraph

    -- Perform the graph traversal starting from the direct parents.
    -- The initial 'visited' set is the same as the initial 'toVisit' set (the direct parents).
    -- The result 'allAncestors' will be the set of all colors that can eventually contain the target color.
    let allAncestors = findAncestors reverseGraph directParents directParents

    -- Print the total number of unique ancestor bag colors found.
    print (Set.size allAncestors)
