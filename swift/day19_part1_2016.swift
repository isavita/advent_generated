import Foundation

let totalElves = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let winner = findWinningElf(totalElves: Int(totalElves)!)

print(winner)

func findWinningElf(totalElves: Int) -> Int {
    var highestPowerOfTwo = 1
    while highestPowerOfTwo * 2 <= totalElves {
        highestPowerOfTwo *= 2
    }
    return (totalElves - highestPowerOfTwo) * 2 + 1
}