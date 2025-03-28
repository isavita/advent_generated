
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), (|>))
import Data.Foldable (toList)
import System.IO (readFile)
import Data.List (break)

-- Type alias for clarity
type Deck = Seq Int
type GameState = (Deck, Deck)

-- | Parses the input string into two decks for Player 1 and Player 2.
-- Assumes the input format:
-- Player 1:
-- <card1>
-- ...
-- <cardN>
--
-- Player 2:
-- <cardA>
-- ...
-- <cardZ>
parseDecks :: String -> GameState
parseDecks input =
    let allLines = lines input
        -- Find the blank line separating the decks
        (p1LinesRaw, rest) = break null allLines
        -- Skip the blank line itself and get Player 2 lines
        p2LinesRaw = drop 1 rest

        -- Extract card numbers, skipping header lines ("Player 1:", "Player 2:")
        p1Cards = map read . tail $ p1LinesRaw
        p2Cards = map read . tail $ p2LinesRaw

    in (Seq.fromList p1Cards, Seq.fromList p2Cards)

-- | Plays a single round of Combat.
playRound :: GameState -> GameState
playRound (p1Deck, p2Deck) =
    -- Use viewl for safe O(1) access to the front element (top card)
    case (Seq.viewl p1Deck, Seq.viewl p2Deck) of
        -- Both players have cards
        (p1Card :< p1Rest, p2Card :< p2Rest) ->
            if p1Card > p2Card
                -- Player 1 wins: adds both cards to the bottom of their deck
                then (p1Rest |> p1Card |> p2Card, p2Rest)
                -- Player 2 wins: adds both cards to the bottom of their deck
                else (p1Rest, p2Rest |> p2Card |> p1Card)
        -- Should not happen during a valid game according to rules,
        -- as the game ends when a deck becomes empty *before* drawing.
        -- This case indicates an issue if reached via playGame.
        _ -> error "playRound called with an empty deck unexpectedly."

-- | Plays the game of Combat until one player has all the cards.
playGame :: GameState -> GameState
playGame state@(p1Deck, p2Deck)
    -- Check termination condition: one deck is empty
    | Seq.null p1Deck || Seq.null p2Deck = state
    -- Continue playing rounds recursively
    | otherwise = playGame (playRound state)

-- | Calculates the score of the winning deck.
calculateScore :: Deck -> Int
calculateScore deck =
    let n = Seq.length deck
        -- Convert sequence to list for easy zipping with multipliers
        cards = toList deck
        -- Multipliers are n, n-1, ..., 1 corresponding to top-to-bottom cards
        multipliers = [n, n-1 .. 1]
    in sum $ zipWith (*) cards multipliers

-- | Main entry point of the program.
main :: IO ()
main = do
    -- Read input from the specified file
    input <- readFile "input.txt"

    -- Parse the initial decks
    let initialState = parseDecks input

    -- Simulate the game to get the final state
    let finalState@(finalP1, finalP2) = playGame initialState

    -- Determine the winner's deck (the non-empty one)
    let winnerDeck = if Seq.null finalP1 then finalP2 else finalP1

    -- Calculate and print the winner's score
    let score = calculateScore winnerDeck
    print score

