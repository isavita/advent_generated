
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

for _ in 0..<20 {
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
}

var sum = 0
for k in state.keys {
    sum += k
}

print(sum)

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
