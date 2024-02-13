
import Foundation

let input = try String(contentsOfFile: "input.txt")
let numbers = input.components(separatedBy: "\n").compactMap { Int($0) }

func findInvalidNumber(numbers: [Int], preambleLength: Int) -> Int {
    for i in preambleLength..<numbers.count {
        let preamble = Array(numbers[i - preambleLength..<i])
        var found = false
        for j in 0..<preamble.count {
            for k in j+1..<preamble.count {
                if preamble[j] + preamble[k] == numbers[i] {
                    found = true
                    break
                }
            }
            if found {
                break
            }
        }
        if !found {
            return numbers[i]
        }
    }
    return -1
}

let invalidNumber = findInvalidNumber(numbers: numbers, preambleLength: 25)
print(invalidNumber)
