
import Foundation

let input = try String(contentsOfFile: "input.txt").components(separatedBy: .whitespacesAndNewlines)
let players = Int(input[0])!
let lastMarble = Int(input[6])!

var scores = Array(repeating: 0, count: players)
var circle = [0]
var currentMarbleIndex = 0

for marble in 1...lastMarble {
    if marble % 23 == 0 {
        let player = marble % players
        scores[player] += marble
        currentMarbleIndex = (currentMarbleIndex - 7 + circle.count) % circle.count
        scores[player] += circle.remove(at: currentMarbleIndex)
    } else {
        currentMarbleIndex = (currentMarbleIndex + 2) % circle.count
        if currentMarbleIndex == 0 {
            circle.append(marble)
            currentMarbleIndex = circle.count - 1
        } else {
            circle.insert(marble, at: currentMarbleIndex)
        }
    }
}

print(scores.max()!)
