
import Foundation

// Function to read the decks from the input file
func readDecks(from file: String) -> ([Int], [Int]) {
    let fileURL = URL(fileURLWithPath: file)
    guard let content = try? String(contentsOf: fileURL, encoding: .utf8) else {
        fatalError("Could not read from file: \(file)")
    }

    let parts = content.components(separatedBy: "\n\n")
    let player1 = parts[0].components(separatedBy: .newlines).dropFirst().compactMap { Int($0) }
    let player2 = parts[1].components(separatedBy: .newlines).dropFirst().compactMap { Int($0) }

    return (Array(player1), Array(player2))
}

// Function to calculate the score of the winning deck
func calculateScore(deck: [Int]) -> Int {
    return deck.reversed().enumerated().reduce(0) { (result, arg1) in
        let (index, value) = arg1
        return result + (value * (index + 1))
    }
}

// Function to play a game of Combat (Part 1)
func playCombat(deck1: [Int], deck2: [Int]) -> (winner: Int, winningDeck: [Int]) {
    var player1Deck = deck1
    var player2Deck = deck2

    while !player1Deck.isEmpty && !player2Deck.isEmpty {
        let card1 = player1Deck.removeFirst()
        let card2 = player2Deck.removeFirst()

        if card1 > card2 {
            player1Deck.append(contentsOf: [card1, card2])
        } else {
            player2Deck.append(contentsOf: [card2, card1])
        }
    }

    return player1Deck.isEmpty ? (2, player2Deck) : (1, player1Deck)
}

// Function to play a game of Recursive Combat (Part 2)
func playRecursiveCombat(deck1: [Int], deck2: [Int]) -> (winner: Int, winningDeck: [Int]) {
    var player1Deck = deck1
    var player2Deck = deck2
    var previousRounds = Set<[[Int]]>()

    func recursiveCombat(deck1: inout [Int], deck2: inout [Int]) -> Int {
        var previousRounds = Set<[[Int]]>()  // Local to this game

        while !deck1.isEmpty && !deck2.isEmpty {
            let roundState = [deck1, deck2]
            if previousRounds.contains(roundState) {
                return 1 // Player 1 wins
            }
            previousRounds.insert(roundState)

            let card1 = deck1.removeFirst()
            let card2 = deck2.removeFirst()

            let roundWinner: Int
            if deck1.count >= card1 && deck2.count >= card2 {
                // Recurse
                var subDeck1 = Array(deck1[0..<card1])
                var subDeck2 = Array(deck2[0..<card2])
                roundWinner = recursiveCombat(deck1: &subDeck1, deck2: &subDeck2)
            } else {
                roundWinner = card1 > card2 ? 1 : 2
            }

            if roundWinner == 1 {
                deck1.append(contentsOf: [card1, card2])
            } else {
                deck2.append(contentsOf: [card2, card1])
            }
        }

        return deck1.isEmpty ? 2 : 1
    }


    let overallWinner = recursiveCombat(deck1: &player1Deck, deck2: &player2Deck)
        let winningDeck = overallWinner == 1 ? player1Deck : player2Deck
        return (overallWinner, winningDeck)
}


// Main function
func main() {
    let (initialDeck1, initialDeck2) = readDecks(from: "input.txt")

    // Part 1
    let (winner, winningDeck) = playCombat(deck1: initialDeck1, deck2: initialDeck2)
    let score = calculateScore(deck: winningDeck)
    print("Part 1 - Winning Player's Score: \(score)")

    // Part 2
    let (recursiveWinner, recursiveWinningDeck) = playRecursiveCombat(deck1: initialDeck1, deck2: initialDeck2)
    let recursiveScore = calculateScore(deck: recursiveWinningDeck)
    print("Part 2 - Winning Player's Score: \(recursiveScore)")
}

// Set the main entry point
main()

