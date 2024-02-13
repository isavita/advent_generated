
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
var checksum = 0
var evenlyDivisibleSum = 0

let rows = input.components(separatedBy: .newlines)
for row in rows {
    let numbers = row.components(separatedBy: .whitespaces).compactMap { Int($0) }
    let maxNum = numbers.max() ?? 0
    let minNum = numbers.min() ?? 0
    checksum += maxNum - minNum
    
    for i in 0..<numbers.count {
        for j in i+1..<numbers.count {
            if numbers[i] % numbers[j] == 0 {
                evenlyDivisibleSum += numbers[i] / numbers[j]
            } else if numbers[j] % numbers[i] == 0 {
                evenlyDivisibleSum += numbers[j] / numbers[i]
            }
        }
    }
}

print(checksum)
print(evenlyDivisibleSum)
