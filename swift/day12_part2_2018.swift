
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

var initialState = ""
var rules = [String: Character]()

for line in lines {
    if line.contains("initial state") {
        initialState = line.components(separatedBy: ": ")[1]
    } else if line.contains("=>") {
        let parts = line.components(separatedBy: " => ")
        rules[parts[0]] = parts[1].first!
    }
}

var state = [Int: Character]()
for (i, c) in initialState.enumerated() {
    if c == "#" {
        state[i] = "#"
    }
}

var previousPattern = ""
var previousSum = 0
var offset = 0
for generation in 0..<50000000000 {
    var newState = [Int: Character]()
    let (minPot, maxPot) = minMaxKeys(state)
    for i in (minPot - 2)...(maxPot + 2) {
        var pattern = ""
        for j in (i - 2)...(i + 2) {
            if state[j] == "#" {
                pattern += "#"
            } else {
                pattern += "."
            }
        }
        if rules[pattern] == "#" {
            newState[i] = "#"
        }
    }
    state = newState

    let (currentPattern, currentSum) = statePattern(state)
    if currentPattern == previousPattern {
        offset = currentSum - previousSum
        let remainingGenerations = 50000000000 - generation - 1
        let finalSum = currentSum + offset * remainingGenerations
        print(finalSum)
        break
    }
    previousPattern = currentPattern
    previousSum = currentSum
}

func minMaxKeys(_ m: [Int: Character]) -> (Int, Int) {
    var first = true
    var minKey = 0
    var maxKey = 0
    for k in m.keys {
        if first {
            minKey = k
            maxKey = k
            first = false
        } else {
            if k < minKey {
                minKey = k
            }
            if k > maxKey {
                maxKey = k
            }
        }
    }
    return (minKey, maxKey)
}

func statePattern(_ m: [Int: Character]) -> (String, Int) {
    let (minPot, maxPot) = minMaxKeys(m)
    var pattern = ""
    var sum = 0
    for i in minPot...maxPot {
        if m[i] == "#" {
            pattern += "#"
            sum += i
        } else {
            pattern += "."
        }
    }
    return (pattern, sum)
}
