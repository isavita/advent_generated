
import Foundation

let input = try String(contentsOfFile: "input.txt").components(separatedBy: .newlines).compactMap { Int($0) }

func findInvalidNumber(_ numbers: [Int], preambleLength: Int) -> Int {
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

func findContiguousSet(_ numbers: [Int], target: Int) -> [Int] {
    for i in 0..<numbers.count {
        var sum = numbers[i]
        var j = i + 1
        while sum < target {
            sum += numbers[j]
            j += 1
        }
        if sum == target {
            return Array(numbers[i..<j])
        }
    }
    return []
}

let invalidNumber = findInvalidNumber(input, preambleLength: 25)
let contiguousSet = findContiguousSet(input, target: invalidNumber)
let encryptionWeakness = contiguousSet.min()! + contiguousSet.max()!

print(encryptionWeakness)
