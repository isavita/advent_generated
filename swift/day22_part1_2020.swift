
import Foundation

let input = try String(contentsOfFile: "input.txt")
let players = input.components(separatedBy: "\n\n")

var player1 = players[0].components(separatedBy: "\n").dropFirst().compactMap { Int($0) }
var player2 = players[1].components(separatedBy: "\n").dropFirst().compactMap { Int($0) }

while !player1.isEmpty && !player2.isEmpty {
    let card1 = player1.removeFirst()
    let card2 = player2.removeFirst()
    
    if card1 > card2 {
        player1.append(contentsOf: [card1, card2])
    } else {
        player2.append(contentsOf: [card2, card1])
    }
}

let winningDeck = player1.isEmpty ? player2 : player1
let score = winningDeck.reversed().enumerated().reduce(0) { $0 + $1.element * ($1.offset + 1) }
print(score)
