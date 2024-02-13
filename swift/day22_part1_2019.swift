
import Foundation

let input = try String(contentsOfFile: "input.txt")
let instructions = input.components(separatedBy: "\n")

var deck = Array(0...10006)

for instruction in instructions {
    if instruction.hasPrefix("deal into new stack") {
        deck.reverse()
    } else if instruction.hasPrefix("cut") {
        let n = Int(instruction.components(separatedBy: " ")[1])!
        if n > 0 {
            deck = Array(deck[n...] + deck[0..<n])
        } else {
            deck = Array(deck[(deck.count + n)...] + deck[0..<(deck.count + n)])
        }
    } else if instruction.hasPrefix("deal with increment") {
        let n = Int(instruction.components(separatedBy: " ")[3])!
        var newDeck = Array(repeating: -1, count: deck.count)
        var index = 0
        for card in deck {
            newDeck[index] = card
            index = (index + n) % deck.count
        }
        deck = newDeck
    }
}

if let position = deck.firstIndex(of: 2019) {
    print(position)
}
