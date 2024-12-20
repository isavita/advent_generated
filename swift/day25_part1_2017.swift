
import Foundation

func parseInput(filePath: String) -> (String, Int, [String: [Int: (Int, Int, String)]]) {
    guard let fileContent = try? String(contentsOfFile: filePath) else { fatalError() }
    let lines = fileContent.components(separatedBy: "\n")
    let initialState = String(lines[0].suffix(2).prefix(1))
    let steps = Int(lines[1].filter("0123456789".contains))!
    var states: [String: [Int: (Int, Int, String)]] = [:]
    var i = 3
    while i < lines.count {
        let state = String(lines[i].suffix(2).prefix(1))
        let value0 = Int(String(lines[i+2].suffix(2).prefix(1)))!
        let move0 = lines[i+3].hasSuffix("left.") ? -1 : 1
        let nextState0 = String(lines[i+4].suffix(2).prefix(1))
        let value1 = Int(String(lines[i+6].suffix(2).prefix(1)))!
        let move1 = lines[i+7].hasSuffix("left.") ? -1 : 1
        let nextState1 = String(lines[i+8].suffix(2).prefix(1))
        states[state] = [0: (value0, move0, nextState0), 1: (value1, move1, nextState1)]
        i += 10
    }
    return (initialState, steps, states)
}

func runTuringMachine(filePath: String) -> Int {
    let (initialState, steps, states) = parseInput(filePath: filePath)
    var tape: [Int: Int] = [:]
    var cursor = 0
    var state = initialState

    for _ in 0..<steps {
        let value = tape[cursor, default: 0]
        let (newValue, move, nextState) = states[state]![value]!
        tape[cursor] = newValue
        cursor += move
        state = nextState
    }

    return tape.values.reduce(0, +)
}

let result = runTuringMachine(filePath: "input.txt")
print(result)
