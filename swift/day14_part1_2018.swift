
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let target = Int(input)!

var scoreboard = [3, 7]
var elf1 = 0
var elf2 = 1

while scoreboard.count < target + 10 {
    let sum = scoreboard[elf1] + scoreboard[elf2]
    if sum >= 10 {
        scoreboard.append(1)
    }
    scoreboard.append(sum % 10)

    elf1 = (elf1 + 1 + scoreboard[elf1]) % scoreboard.count
    elf2 = (elf2 + 1 + scoreboard[elf2]) % scoreboard.count
}

let result = scoreboard[target..<target+10].map { String($0) }.joined()
print(result)
