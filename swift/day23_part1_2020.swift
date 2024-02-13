
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
var cups = input.map { Int(String($0)) ?? 0 }

var currentCup = cups[0]
let maxCup = cups.max()!
let minCup = cups.min()!

for _ in 1...100 {
    let pickedUp = [cups.remove(at: (cups.firstIndex(of: currentCup)! + 1) % cups.count),
                    cups.remove(at: (cups.firstIndex(of: currentCup)! + 1) % cups.count),
                    cups.remove(at: (cups.firstIndex(of: currentCup)! + 1) % cups.count)]
    
    var destination = currentCup - 1
    while pickedUp.contains(destination) || destination < minCup {
        destination -= 1
        if destination < minCup {
            destination = maxCup
        }
    }
    
    let destinationIndex = (cups.firstIndex(of: destination)! + 1) % cups.count
    cups.insert(contentsOf: pickedUp, at: destinationIndex)
    
    currentCup = cups[(cups.firstIndex(of: currentCup)! + 1) % cups.count]
}

let result = cups[(cups.firstIndex(of: 1)! + 1)...] + cups[..<cups.firstIndex(of: 1)!]
print(result.map { String($0) }.joined())
