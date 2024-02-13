import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

var scoreboard = [3, 7]
var elf1 = 0
var elf2 = 1
let inputLen = input.count
var inputSequence = [Int]()

for i in 0..<inputLen {
    inputSequence.append(Int(String(input[input.index(input.startIndex, offsetBy: i)]))!)
}

func checkSequence(scoreboard: [Int], sequence: [Int]) -> Bool {
    if scoreboard.count < sequence.count {
        return false
    }
    let start = scoreboard.count - sequence.count
    for i in 0..<sequence.count {
        if scoreboard[start + i] != sequence[i] {
            return false
        }
    }
    return true
}

while true {
    let newScore = scoreboard[elf1] + scoreboard[elf2]
    if newScore >= 10 {
        scoreboard.append(newScore / 10)
        if checkSequence(scoreboard: scoreboard, sequence: inputSequence) {
            break
        }
    }
    scoreboard.append(newScore % 10)
    if checkSequence(scoreboard: scoreboard, sequence: inputSequence) {
        break
    }

    elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.count
    elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.count
}

print(scoreboard.count - inputLen)