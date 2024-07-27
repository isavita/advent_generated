
import Foundation

// Function to calculate the score for a single round
func calculateRoundScore(opponent: String, player: String) -> Int {
    let shapeScores = ["X": 1, "Y": 2, "Z": 3] // Player's shape scores
    let outcomeScores = ["AX": 3, "AY": 6, "AZ": 0, // Opponent plays Rock
                         "BX": 0, "BY": 3, "BZ": 6, // Opponent plays Paper
                         "CX": 6, "CY": 0, "CZ": 3] // Opponent plays Scissors
    
    let playerShape = player
    let opponentShape = opponent
    
    // Calculate the score for the shape chosen by the player
    let shapeScore = shapeScores[playerShape] ?? 0
    
    // Calculate the outcome score based on both players' choices
    let outcomeScore = outcomeScores[opponentShape + playerShape] ?? 0
    
    return shapeScore + outcomeScore
}

// Main function to read input and calculate total score
func calculateTotalScore(from file: String) -> Int {
    do {
        let contents = try String(contentsOfFile: file)
        let rounds = contents.split(separator: "\n")
        
        var totalScore = 0
        
        for round in rounds {
            let moves = round.split(separator: " ")
            if moves.count == 2 {
                let opponentMove = String(moves[0])
                let playerMove = String(moves[1])
                totalScore += calculateRoundScore(opponent: opponentMove, player: playerMove)
            }
        }
        
        return totalScore
    } catch {
        print("Error reading file: \(error)")
        return 0
    }
}

// Specify the input file
let inputFile = "input.txt"

// Calculate and print the total score
let totalScore = calculateTotalScore(from: inputFile)
print("Total Score: \(totalScore)")
